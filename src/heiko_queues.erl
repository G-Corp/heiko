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
  heiko_registry:register_queue(Name),
  case heiko_queues_sup:start_child([Name, Params]) of
    {ok, QueuePid} ->
      QueueRef = erlang:monitor(process, QueuePid),
      {reply, ok, {Queues#{Name => #{pid => QueuePid, ref => QueueRef, params => Params}},
                   Refs#{QueueRef => Name}}};
    Other ->
      {reply, Other, State}
  end;
handle_call({update_queue, Name, Params}, From, State) ->
  case heiko_registry:queue_monitor(Name) of
    undefined ->
      {reply, {error, invalid_queue}, State};
    Pid ->
      gen_server:cast(Pid, {update, From, Params}),
      {noreply, State}
  end;
handle_call({queue, Name, Fun, Args}, From, State) ->
  case heiko_registry:queue_monitor(Name) of
    undefined ->
      {reply, {error, invalid_queue}, State};
    Pid ->
      gen_server:cast(Pid, {queue, From, Fun, Args}),
      {noreply, State}
  end;
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

% @hidden
handle_cast({delete_queue, Name}, State) ->
  gen_server:cast(heiko_registry:queue_monitor(Name), delete),
  {noreply, State};
handle_cast({terminate_queue, Name}, State) ->
  supervisor:terminate_child(heiko_queues_sup, whereis(Name)),
  heiko_registry:unregister_queue(Name),
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
