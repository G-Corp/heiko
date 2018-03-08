% @hidden
-module(heiko_queue_workers_sup).
-compile([{parse_transform, lager_transform}]).
-behaviour(supervisor).

-export([
         start_link/2
         , stop_child/2
         , start_child/2
        ]).
-export([init/1]).

start_link(Name, Args) ->
  supervisor:start_link(?MODULE, [Name, Args]).

stop_child(SupervisorPid, WorkerPid) when is_pid(SupervisorPid), is_pid(WorkerPid) ->
  supervisor:terminate_child(SupervisorPid, WorkerPid).

start_child(SupervisorPid, Args) when is_pid(SupervisorPid) ->
  case supervisor:start_child(SupervisorPid, [Args]) of
    {ok, Child, _} -> {ok, Child};
    Other -> Other
  end.

init([Name, Args]) ->
  heiko_registry:queue_supervisor(Name, self()),
  lager:debug("Start worker supervisor for queue ~p with ~p", [Name, Args]),
  {ok, {
     #{strategy => simple_one_for_one,
       intensity => 0,
       period => 1},
     [
      #{id => heiko_queue_worker,
         start => {heiko_queue_worker, start_link, []},
         type => worker,
         shutdown => 5000}
     ]
    }}.
