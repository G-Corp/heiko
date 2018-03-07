% @hidden
-module(heiko_queues).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1
         , handle_call/3
         , handle_cast/2
         , handle_info/2
         , terminate/2
         , code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% @hidden
init(_Args) ->
  {ok, {#{}, #{}}}.

% @hidden
handle_call({create_queue, Name, Params}, _From, {Queues, Refs} = State) ->
  case heiko_queues_sup:start_child([Name, Params]) of
    {ok, QueuePid} ->
      QueueRef = erlang:monitor(process, QueuePid),
      {reply, ok, {Queues#{Name => #{pid => QueuePid, ref => QueueRef, params => Params}},
                   Refs#{QueueRef => Name}}};
    Other ->
      {reply, Other, State}
  end;
handle_call({update_queue, Name, Params}, _From, {Queues, _Refs} = State) ->
  {reply,
   gen_server:call(maps:get(monitor, maps:get(Name, Queues)), {update, Params}),
   State};
handle_call({queue, Queue, Fun, Args}, From, {Queues, _Refs} = State) ->
  case maps:get(Queue, Queues, undefined) of
    undefined ->
      {reply, {error, invalid_queue}, State};
    #{monitor := Pid} ->
      gen_server:cast(Pid, {queue, From, Fun, Args}),
      {noreply, State}
  end;
handle_call({queue_size, Queue}, _From, {Queues, _Refs} = State) ->
  case maps:get(Queue, Queues, undefined) of
    undefined ->
      {reply, 0, State};
    #{monitor := Pid} ->
      {reply, gen_server:call(Pid, queue_size), State}
  end;
handle_call({workers, Queue}, _From, {Queues, _Refs} = State) ->
  case maps:get(Queue, Queues, undefined) of
    undefined ->
      {reply, 0, State};
    #{monitor := Pid} ->
      {reply, gen_server:call(Pid, workers), State}
  end;
handle_call({get_pid, Name, Type}, _From, {Queues, _Refs} = State) ->
  {reply, maps:get(Type, maps:get(Name, Queues)), State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

% @hidden
handle_cast({register, Name, Type, Pid}, {Queues, Refs}) ->
  Queue = maps:get(Name, Queues),
  lager:debug("Register ~p (~p) for ~p", [Type, Pid, Name]),
  {noreply, {Queues#{Name => Queue#{Type => Pid}}, Refs}};
handle_cast({delete_queue, Name}, {Queues, _Refs} = State) ->
  gen_server:cast(maps:get(monitor, maps:get(Name, Queues)), delete),
  {noreply, State};
handle_cast({terminate_queue, Name}, State) ->
  supervisor:terminate_child(heiko_queues_sup, whereis(Name)),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

% @hidden
handle_info({'DOWN', Ref, process, _Object, _Reason}, {Queues, Refs}) ->
  Name = maps:get(Ref, Refs),
  _QueueInfo = maps:get(Name, Queues),
  erlang:demonitor(Ref),
  {noreply, {maps:remove(Name, Queues), maps:remove(Ref, Refs)}};
handle_info(_Info, State) ->
  {noreply, State}.

% @hidden
terminate(_Reason, _State) ->
  ok.

% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
