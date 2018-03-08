% @hidden
-module(heiko_queues_sup).
-behaviour(supervisor).

-export([
         start_link/0
         , stop_child/1
         , start_child/1
        ]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop_child(Pid) when is_pid(Pid) ->
  supervisor:terminate_child(?MODULE, Pid).

start_child(Args) ->
  case supervisor:start_child(?MODULE, [Args]) of
    {ok, Child, _} ->
      {ok, Child};
    Other ->
      Other
  end.

init([]) ->
  {ok, {
     #{strategy => simple_one_for_one,
       intensity => 0,
       period => 1},
     [
      #{id => heiko_queue_sup,
        start => {heiko_queue_sup, start_link, []},
        type => supervisor,
        shutdown => 5000}
     ]
    }}.
