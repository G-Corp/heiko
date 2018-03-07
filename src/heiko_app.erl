% @hidden
-module(heiko_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  heiko_sup:start_link().

stop(_State) ->
  ok.
