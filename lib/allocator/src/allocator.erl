-module(allocator).

-behaviour(gen_server).

-export([start_link/0,subordinate/1,stop/0]).

-export([grab/3,release_callback/1]).

-export([reserve/1,release/1]).

-export([write/3,update/3,lookup/1,cancel/1]).

-export([info/0,mem_info/0,debug/1]).

-export([available/1,set_debug_flag/1]).


-export( [  init/1 ,
     handle_call/3 ,
     handle_cast/2 ,
     handle_info/2 ,
       terminate/2 ,
     code_change/3 ] ).

-export([to_intern_fmt/1]).

-include("allocator_dbg.hrl").

-include("allocator.hrl").

-define(down,proc_down_ets_transfer).
-define(srv_tbl,allocator_data_table).
-define(million,1000000).
-define(mask_ipv4,16#FFffFFff).
-define(mask_ipv6,16#FFFFffffFFFFffffFFFFffffFFFFffff).
-define(ipv4_mapped_ipv6,16#FFff00000000).
-define(one_hour_in_seconds,3600).
-define(alloc_srv,allocator_gen_srv).
-define(time_out,100). % milliseconds
-define(time_to_live,?time_out * 1000). % microseconds

-define(is_ipv4(X), is_tuple(X)
    andalso 4 == size(X)
    andalso 0 =< element(1,X) andalso element(1,X) < 256
    andalso 0 =< element(2,X) andalso element(2,X) < 256
    andalso 0 =< element(3,X) andalso element(3,X) < 256
    andalso 0 =< element(4,X) andalso element(4,X) < 256 ).

-define(is_ipv6(X), is_tuple(X)
    andalso 8 == size(X)
    andalso 0 =< element(1,X) andalso element(1,X) < 16#10000
    andalso 0 =< element(2,X) andalso element(2,X) < 16#10000
    andalso 0 =< element(3,X) andalso element(3,X) < 16#10000
    andalso 0 =< element(4,X) andalso element(4,X) < 16#10000
    andalso 0 =< element(5,X) andalso element(5,X) < 16#10000
    andalso 0 =< element(6,X) andalso element(6,X) < 16#10000
    andalso 0 =< element(7,X) andalso element(7,X) < 16#10000
    andalso 0 =< element(8,X) andalso element(8,X) < 16#10000 ).


-record(allocator_state,
  { srv_data, hash_self, hash_tbl, free_tbl,
   nbr_nodes, event_tbl, index_tbl }).


start_link() ->
    gen_server:start_link(
      {local,?alloc_srv},?MODULE,[],[]).

stop() ->
    gen_server:cast(?alloc_srv,stop).

subordinate(Args) ->
    ?dbg("Args=~p~n",[Args]),
    Expand = to_intern_fmt(
      [ proplists:lookup(K,Args) || K <- proplists:get_keys(Args) ] ),
    gen_server:start_link(
      {local,?alloc_srv},?MODULE,Expand,[]).

%%%
%%%   Application interface functions
%%%

%%%
%%%   Function grab/3 reserves a number within a specified
%%%   Range, and commits the given Data to the set of used
%%%   numbers.  Unless there is an 'action' specified in
%%%   the Data, the release_callback/1 function ensures
%%%   that any in-use records that failed to be updated
%%%   regularly will be restored to the set of free
%%%   numbers.  If an alternative 'action' is specified, it
%%%   must call release/1 otherwise there will be a
%%%   resource leak!
%%%
release_callback(Data) ->
    ?dbg("release_callback Data=~p~n",[Data]),
    Origin = proplists:get_value(origin,Data),
    gen_server:cast(Origin,{?req_release_hop,Data}).

grab(Ranges,Data,Timeout) ->
    {ok,Nbr} = reserve(Ranges),
    Node = lookup_srv(Nbr),
    %%
    %%  Append the default allocator 'action' when a
    %%  reservation expires, in case the Data does not
    %%  specify the release callback.
    %%
    Default = [ {origin,{?free_srv,Node}},
                {action,{?MODULE,release_callback}},
                {value,Nbr} ],
    write(Nbr,Data ++ Default,Timeout ).

%%%
%%%   Functions reserve/1 and release/1 only operate on the
%%%   set of free numbers, they do not operate on the set
%%%   of used numbers.
%%%
reserve(Ranges) ->
    case [ {T,L,H}
      || {T,L,H} <- Ranges,
         T == primary orelse T == secondary,
         is_integer(L), is_integer(H), L < H ] of
      Ranges ->
        Nbr_nodes = ets:info(?hash_tbl,size),
        gen_server:call( {?free_srv,pick_srv()},
          { ?request_reserve,
            [ {ranges,Ranges},
              {expiry,expiry(?time_to_live)},
              {hops,Nbr_nodes} ] },
          ?time_out );
      _ -> throw({error,badarg})
    end.

release(Value) when is_integer(Value) ->
    gen_server:cast( {?free_srv,lookup_srv(Value)},
      {?req_release_hop,[{value,Value}]}).

%%%
%%%   Functions write/3 and update/3 only operate on the
%%%   set of used numbers.  The numbers specified may or
%%%   may not be members of the set of free numbers. 
%%%   Writing numbers that are still in the free set into
%%%   the used set as duplicate numbers may inflict
%%%   misery and woe upon the foolish programmer.
%%%
write(Index,Data,Timeout) when is_list(Data), is_integer(Timeout) ->
    Expiry = expiry(Timeout),
    gen_server:call({?used_srv,lookup_srv(Index)},
      {?write_event,Index,Data,Expiry}).

update(Index,Data,Timeout) when is_list(Data), is_integer(Timeout) ->
    Expiry = expiry(Timeout),
    gen_server:call( {?used_srv,lookup_srv(Index)},
      {?update_event,Index,Data,Expiry}).

%%%
%%%   Functions lookup/1 and cancel/1 both return the
%%%   current data associated with the given Index number. 
%%%   Calling cancel/1 will not perform any 'action' stored
%%%   in the returned data, meaning that the Index should
%%%   be returned to the set of free numbers explicitely.
%%%
lookup(Index) ->
    rpc:call(lookup_srv(Index),alloc_used,lookup,[Index]).

cancel(Index) ->
    gen_server:call({?used_srv,lookup_srv(Index)},{?cancel_event,Index}).

%%%
%%%   Function info/0 reports the state of the set of free
%%%   numbers across the entire cluster.
%%%
info() ->
    {Nodes,Down} = up_down_nodes(),
    [{_,Ranges}] = ets:lookup(?srv_tbl,ranges),
    {Avail,Lost} = rpc:multicall(Nodes,allocator,available,[Ranges]),
    Named = [ {Nr,[F||X<-Avail,{Na,F}<-X,Na==Nr]} || {Nr,_,_} <- Ranges ],
    Sum = fun(A,Out) -> A + Out end,
    Result =
      [ {Na,[{base,Min},{size,Max-Min},{free,lists:foldl(Sum,0,Av)}]}
        || {Na,Av} <- Named,
           {Nr,Min,Max} <- Ranges,
           Na == Nr ],
    [{lost_nodes,Lost ++ Down}|Result].

mem_info() ->
    Keys = [memory,size],
    Tbls = [?hash_tbl,?free_tbl,?index_tbl,?event_tbl],
    [ {T,[ {K,ets:info(T,K)} || K <- Keys ]} || T <- Tbls ].

%%%
%%%   Function debug/1 toggles the debugging mode on for
%%%   the limited number of seconds given in Timeout.  The
%%%   limit can extend no more than one hour, but values of
%%%   only a few seconds should be considered wise.
%%%
debug(Timeout) when is_integer(Timeout) ->
    {Nodes,_} = up_down_nodes(),
    Servers = [?free_srv,?used_srv,?alloc_srv],
    Msg = {?debug_flag,Timeout},
    _ = [ gen_server:cast({S,N},Msg) || S <- Servers, N <- Nodes ],
    ok.



%%%
%%%   Internal functions
%%%
lookup_srv(Value) ->
    Nbr_nodes = ets:info(?hash_tbl,size),
    Hash = erlang:phash2(Value,Nbr_nodes),
    case ets:lookup(?hash_tbl,Hash) of
      [{_,{nodedown,_}=Down}] ->
        throw({error,Down});
      [{_,Node}] ->
        Node
    end.

pick_srv() ->
    Nbr_nodes = ets:info(?hash_tbl,size),
    Index = random:uniform(Nbr_nodes) - 1,
    pick_srv_next(Index,Nbr_nodes).

pick_srv_next('$end_of_table',Hops) ->
    pick_srv_next(ets:first(?hash_tbl),Hops);
pick_srv_next(Index,Hops) when 0 < Hops ->
    case ets:lookup(?hash_tbl,Index) of
      [{_,{nodedown,_}}] ->
        pick_srv_next(ets:next(?hash_tbl,Index),Hops - 1);
      [{_,Node}] ->
        Node
    end.

expiry(Microseconds) when is_integer(Microseconds), Microseconds > 0 ->
    {A,B,C} = os:timestamp(),
    case C + Microseconds of
      X when X < ?million -> {A,B,X};
      X ->
        case B + X div ?million of
          Y when Y < ?million -> {A,Y,X rem ?million};
          Y -> {C + Y div ?million,Y rem ?million,X rem ?million}
        end
    end.

up_down_nodes() ->
    ets:foldl(
      fun ({_,{nodedown,N}},{Up,Dn}) -> {Up,[N|Dn]};
          ({_,N},{Up,Dn}) -> {[N|Up],Dn} ;
          (_,Out) -> Out end,
      {[],[]},?hash_tbl).

available([]) -> [];
available([{Name,Min,Max}|Rest]) ->
    [{Name,available_each(ets:next(?free_tbl, Min - 1),Max)}|available(Rest)].

available_each(Nbr,Max) when is_integer(Nbr), Nbr < Max ->
    1 + available_each(ets:next(?free_tbl,Nbr),Max);
available_each(_,_) -> 0.

%%%
%%%   Behaviour call-back functions for gen_server
%%%

init(Args) ->
    set_debug_flag(value(debug_on_for_seconds,Args)),
    case value(group_leader,Args) of
      GL when is_pid(GL) -> erlang:group_leader(GL,erlang:self());
      _ -> ok end,
    ?dbg("init(~p)~n",[Args]),

    Tbl_opt = [named_table,protected,{keypos,1},{heir,erlang:self(),?down}],

    Srv_data = ets:new(?srv_tbl,[set|Tbl_opt]),
    true = ets:insert_new(Srv_data,Args),

    Hash_tbl = ets:new(?hash_tbl,[set|Tbl_opt]),
    [{_,Pairs}] = ets:lookup(?srv_tbl,hash_to_node),
    true = ets:insert_new(Hash_tbl,Pairs),
    ?dbg("hash_tbl = ~p~n",[ets:tab2list(Hash_tbl)]),

    [] = [ {error,N} || {_,N} <- Pairs, true /= erlang:monitor_node(N,true) ],

    Hash_self = ets:foldl(
      fun ({I,V},_) when V == node() -> I;
          (_,Out) -> Out end,
      undefined,Hash_tbl),
    ?dbg("Hash_self = ~p~n",[Hash_self]),

    Used_tbl_opt = [named_table,protected,{keypos,2}],
    Event_tbl = ets:new(?event_tbl,[ordered_set|Used_tbl_opt]),
    Index_tbl = ets:new(?index_tbl,[set|Used_tbl_opt]),
    Free_tbl = ets:new(?free_tbl,[ordered_set|Tbl_opt]),
    Nbr_nodes = ets:info(?hash_tbl,size),
    Released = fun (Nbr) ->
      erlang:phash2(Nbr,Nbr_nodes) == Hash_self
        andalso ets:insert_new(?free_tbl,{Nbr,[]})
    end,
    Expiry = expiry(quarantine(Args)),
    Quarantined = fun (Nbr) ->
      erlang:phash2(Nbr,Nbr_nodes) == Hash_self
        andalso begin
          [{_,Node}] = ets:lookup(?hash_tbl,Hash_self),
          Data =
            [ {origin,{?free_srv,Node}},
              {action,{?MODULE,release_callback}},
              {value,Nbr} ],
          Event = #future_event{ expiry = {Expiry,Nbr}, data = Data },
          Xref = #xref{ index = Nbr, value = {Expiry,Nbr} },
          ets:insert_new(?index_tbl,Xref)
            andalso ets:insert_new(?event_tbl,Event)
        end
    end,
    proplists:get_bool(restore_all_free,Args) andalso
      fill_tables(value(ranges,Args),Released) orelse
      fill_tables(value(ranges,Args),Quarantined),

    ?dbg("######   Finished init~n",[]),
    { ok,
      #allocator_state{
         srv_data = Srv_data,
        hash_self = Hash_self,
         free_tbl = Free_tbl,
         hash_tbl = Hash_tbl,
        nbr_nodes = Nbr_nodes,
        event_tbl = Event_tbl,
        index_tbl = Index_tbl } }.

quarantine(Args) ->
    proplists:get_value(
      quarantine_in_seconds,Args,?default_quarantine_period) * ?million.

handle_call(_Msg,_,State) ->
    ?dbg("handle_call ~p~n",[_Msg]),
    {reply,{error,not_recognised},State}.

handle_cast(stop,State) ->
    {stop,normal,State};

handle_cast({?debug_flag,Timeout},State) ->
    set_debug_flag(Timeout),
    {noreply,State};

handle_cast(_X,State) ->
    ?dbg("handle_cast ~p~n",[_X]),
    {noreply,State}.

handle_info({'ETS-TRANSFER',Tbl,Pid,?down}=_Msg,State) ->
    ?dbg("handle_info ~p~n",[_Msg]),
    #allocator_state{srv_data=Srv_data} = State,
    true = ets:insert(Srv_data, [{{tbl_to_pid,Tbl},undefined}]),
    ets:delete(Srv_data,{pid_to_tbl,Pid}),
    {noreply,State};

handle_info({nodedown,Node}=_Msg,State) ->
    %%%   ?dbg("handle_info ~p~n",[_Msg]),
    Index = ets:foldl(
      fun ({I,V},_) when V == Node -> I;
          (_,Out) -> Out end,
      undefined,?hash_tbl),
    Index /= undefined andalso ets:insert(?hash_tbl,{Index,{nodedown,Node}}),
    {noreply,State};



handle_info({?waiting_for_tables,alloc_free,Pid}=_X,State) ->
    ?dbg("handle_info ~p~n",[_X]),
    #allocator_state{srv_data=Srv_data} = State,
    true = ets:insert(Srv_data,
      [{{tbl_to_pid,?free_tbl},Pid}]),
    ets:give_away(?free_tbl,Pid,?restore_free_tbl),
    {noreply,State};

handle_info({?waiting_for_tables,alloc_used,Pid}=_X,State) ->
    ?dbg("handle_info ~p~n",[_X]),
    #allocator_state{srv_data=Srv_data} = State,
    true = ets:insert(Srv_data,
      [{{tbl_to_pid,?index_tbl},Pid},
       {{tbl_to_pid,?event_tbl},Pid}]),
    ets:give_away(?index_tbl,Pid,?restore_index_tbl),
    ets:give_away(?event_tbl,Pid,?restore_event_tbl),
    {noreply,State};


handle_info(_X,State) ->
    ?dbg("handle_info ~p~n",[_X]),
    {noreply,State}.

terminate(_Reason,_State) ->
    ?dbg("terminate ~p~n",[_Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok,State}.

%%%
%%%   Internal functions
%%%


fill_tables([],_) -> true;
fill_tables([{Name,Nbr,Max}|Rest],Belongs) when Nbr < Max ->
    Belongs(Nbr),
    fill_tables([{Name,Nbr + 1,Max}|Rest],Belongs);
fill_tables([_|Rest],Belongs) ->
    fill_tables(Rest,Belongs).

value(Keys,Info) when is_list(Keys) ->
    [ proplists:get_value(K,Info) || K <- Keys ];
value(Key,Info) ->
    proplists:get_value(Key,Info).

set_debug_flag(Timeout) ->
    if is_integer(Timeout), 0 < Timeout, Timeout =< ?one_hour_in_seconds ->
         erlang:put(?debug_flag,expiry(Timeout * ?million)),
         ?dbg("Debugging activated for ~p seconds.~n",[Timeout]);
       true -> erlang:erase(?debug_flag) end.

to_intern_fmt([]) -> [];
to_intern_fmt([{ranges,Ranges}|Rest]) ->
    [ {ranges,[each_range(R)||R<-Ranges]} | to_intern_fmt(Rest) ];
to_intern_fmt([{Name,Value}|Rest]) -> [{Name,Value}|to_intern_fmt(Rest) ].


each_range({Name,Subnet}) when is_atom(Name), is_list(Subnet) ->
    Mask = mask_part(Subnet),
    Addr = addr_part(Subnet),
    case inet:parse_ipv6strict_address(Addr) of
      {error,einval} ->
        {ok,IPv4} = inet:parse_ipv4strict_address(Addr),
        Bits = ?mask_ipv4 bxor (?mask_ipv4 bsr Mask),
        Lo = ( ?mask_ipv4 band (ip_to_int(IPv4) band Bits) )
             + ?ipv4_mapped_ipv6,
        Hi = 1 + Lo + (?mask_ipv4 bxor Bits);
      {ok,IPv6} ->
        Bits = ?mask_ipv6 bxor (?mask_ipv6 bsr Mask),
        Lo = ?mask_ipv6 band (ip_to_int(IPv6) band Bits),
        Hi = 1 + Lo + (?mask_ipv6 bxor Bits)
    end,
    {Name,Lo,Hi};
each_range({Name,Lo,Hi}) when is_atom(Name), ?is_ipv4(Lo), ?is_ipv4(Hi) ->
    A = ip_to_int(Lo),
    B = ip_to_int(Hi),
    true = A < B,
    {Name,A,B};
each_range({Name,Lo,Hi}) when is_atom(Name), ?is_ipv6(Lo), ?is_ipv6(Hi) ->
    A = ip_to_int(Lo),
    B = ip_to_int(Hi),
    true = A < B,
    {Name,A,B};
each_range({Name,Lo,Hi}) when is_atom(Name), is_list(Lo), is_list(Hi) ->
    case inet:parse_ipv6strict_address(Lo) of
      {error,einval} ->
        {ok,A} = inet:parse_ipv4strict_address(Lo),
        {ok,B} = inet:parse_ipv4strict_address(Hi),
        true = A < B,
        { Name,
          ip_to_int(A) + ?ipv4_mapped_ipv6,
          ip_to_int(B) + ?ipv4_mapped_ipv6 };
      {ok,A} ->
        {ok,B} = inet:parse_ipv6strict_address(Hi),
        true = A < B,
        { Name, ip_to_int(A), ip_to_int(B) }
    end;
each_range({Name,Lo,Hi})
when is_atom(Name)
   , is_integer(Lo), is_integer(Hi)
   , Lo < Hi ->
    {Name,Lo,Hi}.

ip_to_int({A,B,C,D}) ->
    <<Int:32>> = <<A:8,B:8,C:8,D:8>>,
    Int;
ip_to_int({A,B,C,D,E,F,G,H}) ->
    <<Int:128>> = <<A:16,B:16,C:16,D:16,E:16,F:16,G:16,H:16>>,
    Int.

addr_part([$/|_]) -> [];
addr_part([Each|Rest]) -> [Each|addr_part(Rest)].

mask_part([$/|Mask]) -> list_to_integer(Mask);
mask_part([_|Rest]) -> mask_part(Rest).
