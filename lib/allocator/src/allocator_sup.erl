
-module(allocator_sup).

-behaviour(supervisor).

-export([start_link/0,start_link/1]).

-export([init/1]).

-include("allocator_dbg.hrl").
-include("allocator.hrl").

-define( CHILD(Srv,Type,Args),
         {Srv,{Srv,subordinate,Args},permanent,5000,Type,[Srv]} ).

start_link() ->
    supervisor:start_link(?MODULE,[]).

start_link(Args) when is_list(Args) ->
    supervisor:start_link(?MODULE,Args).

init(Args) when is_list(Args)->
    Default = application:get_all_env(),
    Supvis = [ {supervisor,erlang:self()} | Args ++ Default ],
    ?dbg("Supvis=~p~n",[Supvis]),
    Allocator = ?CHILD(allocator,worker,[Supvis]),
    Alloc_free = ?CHILD(alloc_free,worker,[Supvis]),
    Alloc_used = ?CHILD(alloc_used,worker,[Supvis]),
    {ok,{{one_for_one,10,1}, [Allocator,Alloc_free,Alloc_used]}}.
