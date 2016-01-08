-module(allocator_guardian).

-behaviour(gen_server).

-export([subordinate/1]).

-export( [  init/1 ,
     handle_call/3 ,
     handle_cast/2 ,
     handle_info/2 ,
       terminate/2 ,
     code_change/3 ] ).

-include("allocator_dbg.hrl").
-include("allocator.hrl").

-record(state,
  { free_tbl,
    hash_tbl } ).

-define(down,table_is_released).
-define(restore_hash_tbl,restore_hash_tbl).

-define(guardian,gen_server_allocator_guardian).

subordinate(Args) ->
    Expand = [ proplists:lookup(K,Args) || K <- proplists:get_keys(Args) ],
    gen_server:start_link({local,?guardian},?MODULE,Expand,[]).

init(Args) ->
    Tbl_opts = [named_table,protected,{keypos,1},{heir,erlang:self(),?down}],

    Hash_tbl = ets:new(?hash_tbl,[set|Tbl_opts]),
    Pairs = proplists:get_value(hash_to_node,Args),
    true = ets:insert_new(?hash_tbl,Pairs),
    ?dbg("hash_tbl = ~p~n",[ets:tab2list(?hash_tbl)]),

    Hash_self = ets:foldl(
      fun ({I,V},_) when V == erlang:node() -> I;
          (_,Out) -> Out end,
      undefined,?hash_tbl),
    ?dbg("Hash_self = ~p~n",[Hash_self]),
    true = undefined /= Hash_self,

    Free_tbl = ets:new(?free_tbl,[ordered_set|Tbl_opts]),
    Nbr_nodes = length(Pairs),
    Belongs = fun (Nbr) -> erlang:phash2(Nbr,Nbr_nodes) == Hash_self end,
    proplists:get_bool(restore_all_free,Args) andalso
      free_data(Free_tbl,proplists:get_value(ranges,Args),Belongs),

    { ok,
      #state{
           free_tbl = Free_tbl,
           hash_tbl = Hash_tbl } }.

free_data(_,[],_) -> true;
free_data(Free_tbl,[{Name,Nbr,Max}|Rest],Belongs) when Nbr < Max ->
    Belongs(Nbr) andalso ets:insert_new(Free_tbl,{Nbr,[]}),
    free_data(Free_tbl,[{Name,Nbr + 1,Max}|Rest],Belongs);
free_data(Free_tbl,[_|Rest],Belongs) ->
    free_data(Free_tbl,Rest,Belongs).


handle_call(_Msg,_,State) ->
    ?dbg("handle_call ~p~n",[_Msg]),
    {reply,{error,not_recognised},State}.

handle_cast(_X,State) ->
    ?dbg("handle_cast ~p~n",[_X]),
    {noreply,State}.

handle_info({'ETS-TRANSFER',_Tbl,_Pid,?down}=_X,State) ->
    ?dbg("handle_info ~p~n",[_X]),
    {noreply,State};

handle_info({?waiting_for_tables,Alloc_pid}=_X,State) ->
    ?dbg("handle_info ~p~n",[_X]),
    #state{
         hash_tbl = Hash_tbl,
         free_tbl = Free_tbl } = State,
    ets:give_away(Hash_tbl,Alloc_pid,?restore_hash_tbl),
    ets:give_away(Free_tbl,Alloc_pid,?restore_free_tbl),
    {noreply,State};


handle_info(_X,State) ->
    ?dbg("handle_info ~p~n",[_X]),
    {noreply,State}.

terminate(_Reason, _State) ->
    ?dbg("terminate ~p~n",[_Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok,State}.
