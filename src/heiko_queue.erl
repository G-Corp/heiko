% @hidden
-module(heiko_queue).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1
         , handle_call/3
         , handle_cast/2
         , handle_info/2
         , terminate/2
         , code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT, #{max_workers => 5}).

start_link(Name, Args) ->
  gen_server:start_link(?MODULE, [Name, Args], []).

% @hidden
init([Name, Args]) ->
  lager:debug("Start monitor for queue ~p with ~p", [Name, Args]),
  heiko_registry:queue_monitor(Name, self()),
  {
   ok,
   maps:merge(
     maps:merge(
       ?DEFAULT,
       maps:from_list(Args)
     ),
     #{workers => [],
       name => Name,
       queue => queue:new(),
       terminate => false}
   )
  }.

% @hidden
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

% @hidden
handle_cast({queue, From, Fun, Args}, #{name := Name, queue := Queue, terminate := false} = State) ->
  gen_server:reply(From, ok),
  Queue1 = queue:in({Fun, Args}, Queue),
  heiko_registry:queue_size(Name, queue:len(Queue1)),
  {noreply, execute(State#{queue => Queue1})};
handle_cast({update, From, Params}, State) ->
  gen_server:reply(From, ok),
  {noreply, maps:merge(State, maps:from_list(Params))};
handle_cast({queue, From, _Fun, _Args}, State) ->
  gen_server:reply(From, {error, queue_terminated}),
  {noreply, State};
handle_cast({terminate, Child}, #{workers := Workers, name := Name} = State) ->
  WorkerSupervisorPid = heiko_registry:queue_supervisor(Name),
  heiko_queue_workers_sup:stop_child(WorkerSupervisorPid, Child),
  Workers1 = lists:delete(Child, Workers),
  heiko_registry:queue_workers(Name, erlang:length(Workers1)),
  {noreply, execute(State#{workers => Workers1})};
handle_cast(delete, State) ->
  erlang:send_after(1000, self(), terminate_queue),
  {noreply, State#{terminate => true}};
handle_cast(_Msg, State) ->
  {noreply, State}.

% @hidden
handle_info(terminate_queue, #{name := Name, workers := []} = State) ->
  gen_server:cast(heiko_queues, {terminate_queue, Name}),
  {noreply, State};
handle_info(terminate_queue, State) ->
  erlang:send_after(1000, self(), terminate_queue),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

% @hidden
terminate(_Reason, _State) ->
  ok.

% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

execute(#{workers := Workers, max_workers := MaxWorkers} = State) when length(Workers) >= MaxWorkers ->
  State;
execute(#{workers := Workers, queue := Queue, name := Name} = State) ->
  case queue:out(Queue) of
    {{value, {Function, Args}}, Queue1} ->
      heiko_registry:queue_size(Name, queue:len(Queue1)),
      WorkerSupervisorPid = heiko_registry:queue_supervisor(Name),
      case heiko_queue_workers_sup:start_child(WorkerSupervisorPid, [self(), Function, Args]) of
        {ok, Child} ->
          Workers1 = [Child|Workers],
          heiko_registry:queue_workers(Name, erlang:length(Workers1)),
          lager:debug("Will execute ~p [~p] supervized by ~p", [Function, Args, WorkerSupervisorPid]),
          gen_server:cast(Child, execute),
          execute(State#{queue => Queue1, workers => Workers1});
        Other ->
          lager:error("Can't start worker for ~p: ~p", [Name, Other]),
          State
      end;
    _ ->
      State
  end.
