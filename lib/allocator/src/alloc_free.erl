-module(alloc_free).

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


-define(req_reserve_hop,req_reserve_hop).
-define(return_reply,return_reply).

-record(allocator_state,{ hash_self, hash_tbl, free_tbl, nbr_nodes }).

%%%
%%%   Application interface functions
%%%

subordinate(Args) ->
    Expand = [ proplists:lookup(K,Args) || K <- proplists:get_keys(Args) ],
    gen_server:start_link(
      {local,?free_srv},?MODULE,Expand,[]).

%%%
%%%   Behaviour call-back functions for gen_server
%%%

init(Args) ->
    allocator:set_debug_flag(proplists:get_value(debug_on_for_seconds,Args)),
    case proplists:get_value(group_leader,Args) of
      GL when is_pid(GL) -> erlang:group_leader(GL,erlang:self());
      _ -> ok end,
    erlang:self() ! {?get_siblings,Args},
    {ok,#allocator_state{}}.



handle_call(_Msg,_From,#allocator_state{free_tbl=undefined}=State) ->
    {reply,{error,no_free_tbl},State};

handle_call({?request_reserve,Info}=_Msg,From,State) ->
    ?dbg("handle_call ~p~n",[_Msg]),
    Origin = {?free_srv,erlang:node()},
    request_reserve([{origin,Origin},{from,From}|Info],State),
    {noreply,State};

handle_call(_Msg,_,State) ->
    ?dbg("handle_call ~p~n",[_Msg]),
    {reply,{error,not_recognised},State}.



handle_cast(stop,State) ->
    {stop,normal,State};

handle_cast({?debug_flag,Timeout},State) ->
    allocator:set_debug_flag(Timeout),
    {noreply,State};

handle_cast(_Msg,#allocator_state{free_tbl=undefined}=State) ->
    {noreply,State};

handle_cast({?req_reserve_hop,Info}=_Msg,State) ->
    ?dbg("handle_cast ~p~n",[_Msg]),
    request_reserve(Info,State),
    {noreply,State};

handle_cast({?req_release_hop,Info}=_Msg,State) ->
    ?dbg("handle_cast ~p~n",[_Msg]),
    Value = value(value,Info),
    _ = ets:insert(?free_tbl,{Value,[]}),
    {noreply,State};

handle_cast({?return_reply,Info}=_Msg,State) ->
    ?dbg("handle_cast ~p~n",[_Msg]),
    [From,Reply] = value([from,reply],Info),
    gen_server:reply(From,Reply),
    {noreply,State};

handle_cast(_X,State) ->
    ?dbg("handle_cast ~p~n",[_X]),
    {noreply,State}.


handle_info({'ETS-TRANSFER',?free_tbl,_Ctrl,restore_free_tbl}=_Msg,State) ->
    ?dbg("handle_info ~p~n",[_Msg]),
    Nbr_nodes = ets:info(?hash_tbl,size),
    Hash_self = ets:foldl(
      fun ({I,N},_) when N == erlang:node() -> I;
          (_,Out) -> Out
      end,
      undefined,?hash_tbl),
    ?dbg("Hash_self = ~p~n",[Hash_self]),
    { noreply,
      State#allocator_state{
        nbr_nodes = Nbr_nodes,
         hash_tbl = ?hash_tbl,
        hash_self = Hash_self,
         free_tbl = ?free_tbl } };


handle_info({nodedown,Node}=_Msg,State) ->
    ?dbg("handle_info ~p~n",[_Msg]),
    #allocator_state{hash_tbl=Hash_tbl} = State,
    Index = ets:foldl(
      fun ({I,V},_) when V == Node -> I;
          (_,Out) -> Out end,
      undefined,Hash_tbl),
    Index /= undefined andalso ets:insert(Hash_tbl,{Index,{nodedown,Node}}),
    {noreply,State};

handle_info({?get_siblings,Args}=_Msg,State) ->
    Supervisor = proplists:get_value(supervisor,Args),
    case is_alive(Supervisor) of
      true ->
        Children = supervisor:which_children(Supervisor),
        Siblings = [ Pid || {_,Pid,_,[Mod]} <- Children, Mod /= ?MODULE ],
        [ P ! {?waiting_for_tables,?MODULE,erlang:self()} || P <- Siblings ],
        {noreply,State};
      _ -> {stop,dead_supervisor}
    end;        

handle_info(_X,State) ->
    ?dbg("handle_info ~p~n",[_X]),
    {noreply,State}.

terminate(_Reason,State) ->
    ?dbg("terminate ~p~n",[_Reason]),
    #allocator_state{free_tbl = Free_tbl, hash_tbl = Hash_tbl} = State,
    ets:delete(Free_tbl),
    ets:delete(Hash_tbl),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok,State}.

%%%
%%%   Internal functions
%%%

request_reserve(Info,State) ->
    Now = os:timestamp(),
    case value([expiry,hops],Info) of
      [Expiry,Hops] when Expiry > Now, Hops > 0 ->
        Local = erlang:node(),
        case seek_number(Info) of
          [{primary,Free},{secondary,{Node,Acquired}}] when Node == Local ->
            true = ets:insert(?free_tbl,{Acquired,[]}),
            reply([{reply,{ok,Free}}|Info]);
          [{primary,Free},{secondary,{Node,Acquired}}] ->
            gen_server:cast({?free_srv,Node},
              {?req_release_hop,[{value,Acquired}|Info]}),
            reply([{reply,{ok,Free}}|Info]);
          [{primary,Free}] ->
            reply([{reply,{ok,Free}}|Info]);
          Other ->
            case next_node(State,Hops) of
              {ok,Next,H} ->
                gen_server:cast({?free_srv,Next},
                  {?req_reserve_hop,[{hops,H - 1}|Other] ++ Info});
              Err ->
                reply([{reply,Err}|Other] ++ Info)
            end
        end;
      _ ->
        case value(secondary,Info) of
          {_Node,Acquired} -> reply([{reply,{ok,Acquired}}|Info]);
          undefined -> reply([{reply,{error,time_out}}|Info])
        end
    end.

next_node(State,Hops) ->
    #allocator_state{ hash_tbl = Hash_tbl, hash_self = Hash_self } = State,
    next_node_each(Hash_tbl,ets:next(Hash_tbl,Hash_self),Hops).

next_node_each(Hash_tbl,'$end_of_table',Hops) ->
    next_node_each(Hash_tbl,ets:first(Hash_tbl),Hops);
next_node_each(Hash_tbl,Key,Hops) when 0 < Hops ->
    case ets:lookup(Hash_tbl,Key) of
      [{_,{nodedown,_}}] ->
        next_node_each(Hash_tbl,ets:next(Hash_tbl,Key),Hops - 1);
      [{_,Next}] -> {ok,Next,Hops}
    end;
next_node_each(_,_,_) -> {error,no_more_nodes}.

seek_number(Info) ->
    case [ {T,V} || {T,V} <- Info, T == secondary ] of
      [] -> Remote_sec = [];
      [X|_] -> Remote_sec = [X]
    end,
    seek_nbr_each(value(ranges,Info),Remote_sec).

seek_nbr_each([],Result) ->
    Result;
seek_nbr_each([{primary,Low,High}|Rest],Result) ->
    case ets:next(?free_tbl,Low - 1) of
      Free when is_integer(Free), Free < High ->
        true = ets:delete(?free_tbl,Free),
        [{primary,Free}|Result];
      _ ->
        seek_nbr_each(Rest,Result)
    end;
seek_nbr_each([{secondary,Low,High}|Rest],[]) ->
    case ets:next(?free_tbl,Low - 1) of
      Free when is_integer(Free), Free < High ->
        true = ets:delete(?free_tbl,Free),
        seek_nbr_each(Rest,[{secondary,{erlang:node(),Free}}]);
      _ ->
        seek_nbr_each(Rest,[])
    end;
seek_nbr_each([{secondary,_,_}|Rest],Result) ->
    seek_nbr_each(Rest,Result).



reply(Info) ->
    Origin = value(origin,Info),
    gen_server:cast(Origin,{?return_reply,Info}).




value(Keys,Info) when is_list(Keys) ->
    [ proplists:get_value(K,Info) || K <- Keys ];
value(Key,Info) ->
    proplists:get_value(Key,Info).


is_alive(Srv) when is_pid(Srv) -> erlang:is_process_alive(Srv);
is_alive(undefined) -> false;
is_alive(Name) -> is_alive(erlang:whereis(Name)).
