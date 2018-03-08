% @hidden
-module(heiko_registry).

-export([
         register_queue/1
         , unregister_queue/1
         , queue_size/2
         , queue_size/1
         , queue_workers/2
         , queue_workers/1
         , queue_monitor/2
         , queue_monitor/1
         , queue_supervisor/2
         , queue_supervisor/1
         , queue_infos/1
        ]).

register_queue(Name) ->
  case lists:member(Name, ets:all()) of
    true ->
      Name;
    _ ->
      ets:new(Name, [public, named_table])
  end.

unregister_queue(Name) ->
  case lists:member(Name, ets:all()) of
    true ->
      ets:delete(Name);
    _ ->
      true
  end.

queue_size(Name, Size) ->
  register_queue(Name),
  ets:insert(Name, [{size, Size}]).

queue_size(Name) ->
  register_queue(Name),
  case ets:lookup(Name, size) of
    [{size, Size}] -> Size;
    _ -> 0
  end.

queue_workers(Name, Size) ->
  register_queue(Name),
  ets:insert(Name, [{workers, Size}]).

queue_workers(Name) ->
  register_queue(Name),
  case ets:lookup(Name, workers) of
    [{workers, Size}] -> Size;
    _ -> 0
  end.

queue_monitor(Name, Pid) ->
  register_queue(Name),
  ets:insert(Name, [{monitor, Pid}]).

queue_monitor(Name) ->
  register_queue(Name),
  case ets:lookup(Name, monitor) of
    [{monitor, Pid}] -> Pid;
    _ -> undefined
  end.

queue_supervisor(Name, Pid) ->
  register_queue(Name),
  ets:insert(Name, [{supervisor, Pid}]).

queue_supervisor(Name) ->
  register_queue(Name),
  case ets:lookup(Name, supervisor) of
    [{supervisor, Pid}] -> Pid;
    _ -> undefined
  end.

queue_infos(Name) ->
  register_queue(Name),
  maps:from_list(ets:tab2list(Name)).
