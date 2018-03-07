% @hidden
-module(heiko_queue_sup).
-compile([{parse_transform, lager_transform}]).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link([Name, Args]) ->
  supervisor:start_link({local, Name}, ?MODULE, [Name, Args]).

init([Name, Args]) ->
  lager:debug("Start queue ~p with ~p", [Name, Args]),
  {ok, {
    #{strategy => one_for_one,
      intensity => 1,
      period => 5},
    [
     #{id => heiko_queue,
       start => {heiko_queue, start_link, [Name, Args]},
       type => worker,
       shutdown => 5000},
     #{id => heiko_queue_workers_sup,
       start => {heiko_queue_workers_sup, start_link, [Name, Args]},
       type => supervisor,
       shutdown => 5000}
    ]
  }}.
