% @hidden
-module(heiko_queue_worker).
-behaviour(gen_server).

%% API
-export([start_link/1]).
-compile([{parse_transform, lager_transform}]).

%% gen_server callbacks
-export([init/1
         , handle_call/3
         , handle_cast/2
         , handle_info/2
         , terminate/2
         , code_change/3]).

-define(SERVER, ?MODULE).

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

% @hidden
init([MonitorPid, Function, Args]) ->
  lager:debug("START WORKER manager: ~p, function: ~p, args: ~p", [MonitorPid, Function, Args]),
  {ok, #{
     monitor => MonitorPid,
     function => Function,
     args => Args
    }}.

% @hidden
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

% @hidden
handle_cast(execute, #{monitor := MonitorPid, function := Function, args := Args} = State) ->
  lager:debug("WORKER run function: ~p, args: ~p", [Function, Args]),
  R = erlang:apply(Function, Args),
  lager:debug("RESPONSE => ~p", [R]),
  gen_server:cast(MonitorPid, {terminate, self()}),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

% @hidden
handle_info(_Info, State) ->
  {noreply, State}.

% @hidden
terminate(_Reason, _State) ->
  ok.

% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
