-module(allocator_app).
-behaviour(application).

-export([start/2,stop/1]).

start(_Type, StartArgs) when is_list(StartArgs) ->
    allocator_sup:start_link(StartArgs).

stop(_State) ->
    ok.
