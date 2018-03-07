-module(heiko).

-export([
         start/0
         , create_queue/2
         , delete_queue/1
         , update_queue/2
         , queue_size/1
         , workers/1
         , queue/3
         , queue/4
        ]).

-type queue() :: atom().
-type options() :: [{max_workers, integer()}].

start() ->
  application:ensure_all_started(heiko).

-spec create_queue(Name::queue(), Params::options()) -> ok | {error, term()}.
create_queue(Name, Params) ->
  gen_server:call(heiko_queues, {create_queue, Name, Params}).

-spec update_queue(Name::queue(), Params::options()) -> ok | {error, term()}.
update_queue(Name, Params) ->
  gen_server:call(heiko_queues, {update_queue, Name, Params}).

-spec delete_queue(Name::queue()) -> ok.
delete_queue(Name) ->
  gen_server:cast(heiko_queues, {delete_queue, Name}).

-spec queue_size(Name::queue()) -> integer().
queue_size(Name) ->
  gen_server:call(heiko_queues, {queue_size, Name}).

-spec workers(Name::queue()) -> integer().
workers(Name) ->
  gen_server:call(heiko_queues, {workers, Name}).

-spec queue(Queue::queue(), Fun::function(), Args::list()) -> ok | {error, term()}.
queue(Queue, Fun, Args) when is_function(Fun, length(Args)) ->
  gen_server:call(heiko_queues, {queue, Queue, Fun, Args}).

-spec queue(Queue::queue(), Module::module(), Function::atom(), Args::list()) -> ok | {error, term()}.
queue(Queue, Module, Function, Args) ->
  queue(Queue, fun(FunArgs) -> erlang:apply(Module, Function, FunArgs) end, [Args]).
