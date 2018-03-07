% @hidden
-module(heiko_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {
    #{strategy => one_for_one,
      intensity => 1,
      period => 5},
    [
     #{id => heiko_queues,
       start => {heiko_queues, start_link, []},
       type => worker,
       shutdown => 5000},
     #{id => heiko_queues_sup,
       start => {heiko_queues_sup, start_link, []},
       type => supervisor,
       shutdown => 5000}
    ]
  }}.
