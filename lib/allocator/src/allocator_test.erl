-module(allocator_test).

-behaviour(gen_server).

-export( [  init/1 ,
     handle_call/3 ,
     handle_cast/2 ,
     handle_info/2 ,
       terminate/2 ,
     code_change/3 ] ).

-export([start_link/0,start_link/1,stop/0]).

-export([test_callback/1,info/0]).

-include("allocator.hrl").
-include("allocator_dbg.hrl").

-define(test_srv,allocator_test_srv).

-define(nbr_nodes,32).
-define(time_out,100). % milliseconds
-define(cache_time_out,(220 * 1000000)). % microseconds

-define(reserve_cycle,0).

-record(test_state,{table,index}).


start_link() ->
    _ = application:start(sasl),
    _ = application:start(os_mon),
    Args = [{ranges,[{primary,"10.128.0.0/17"},{secondary,"10.0.0.0/17"}]}],
    start_link(Args).

start_link(Args) ->
    gen_server:start_link(
      {local,?test_srv},?MODULE,Args,[]).

stop() ->
    gen_server:cast(?test_srv,stop).

test_callback(Data) ->
    Origin = proplists:get_value(origin,Data),
    gen_server:cast(Origin,{cache_record_expired,Data}).

info() ->
    gen_server:call(?test_srv,get_information).


counter_records() ->
    [ {count_1ms,0,0},
      {count_2ms,0,0},
      {count_3ms,0,0},
      {count_4ms,0,0},
      {count_5ms,0,0},
      {count_6ms,0,0},
      {count_7ms,0,0},
      {count_8ms,0,0},
      {count_9ms,0,0},
      {count_10ms,0,0},
      {count_over,0,0},
      {count_reserved,0,0}].

init(Args) ->
    ok = application:load(allocator),
    {ok,Hashes} = application:get_env(allocator,hash_to_node),
    Tbl = ets:new(allocator_test_tbl,[set,protected,{keypos,1},named_table]),
    ets:insert(Tbl,Hashes),
    ets:insert(Tbl,counter_records()),
    Nodes = [ N || {_,N} <- Hashes],
    lists:member(erlang:node(),Nodes)
      orelse throw(test_node_is_outside_of_cluster),

    Ranges = proplists:get_value(ranges,allocator:to_intern_fmt(Args)),
    ets:insert(Tbl,{ranges,Ranges}),

    GL = erlang:group_leader(),
    {_,[]} = rpc:multicall(Nodes,application,load,[allocator]),
    set_app_env( Nodes,
      [ {group_leader,GL},
        {ranges,Ranges},
        {restore_all_free,true} ] ),
    {_,[]} = rpc:multicall(Nodes,application,start,[allocator]),
    allocator:debug(0),

    Index = size_of_ranges(Ranges),
    io:format("~nTest: prepared to proceed with ~p allocations.~n~n",[Index]),
    ets:insert(Tbl,{initial,allocator:info()}),
    ets:insert(Tbl,{start_time,os:timestamp()}),
    {ok,#test_state{table=Tbl,index=Index},?reserve_cycle}.

set_app_env(_,[]) -> ok;
set_app_env(Nodes,[{Name,Value}|Rest]) ->
    {_,[]} = rpc:multicall(Nodes,application,set_env,[allocator,Name,Value]),
    set_app_env(Nodes,Rest).


handle_call(get_information=_Msg,_,State) ->
    ?dbg("handle_call ~p~n",[_Msg]),
    Result = get_information(State),
    #test_state{index=Index} = State,
    case Index of
      0 -> {reply,Result,State};
      _ -> {reply,Result,State,?reserve_cycle}
    end;

handle_call(_Msg,_,State) ->
    ?dbg("handle_call ~p~n",[_Msg]),
    #test_state{index=Index} = State,
    case Index of
      0 -> {reply,{error,not_recognised},State};
      _ -> {reply,{error,not_recognised},State,?reserve_cycle}
    end.


handle_cast(stop,State) ->
    {stop,normal,State};

handle_cast({cache_record_expired,Data}=_X,State) ->
    ?dbg("handle_cast ~p~n",[_X]),
    #test_state{table=Tbl,index=Index} = State,
    Value = proplists:get_value(value,Data),
    allocator:release(Value),
    case ets:update_counter(Tbl,count_reserved,[{2,0},{3,1}]) of
      [Same,Same] ->
        io:format("~nTest: all values released, counted ~p"
                  "~nResult details ~p~n~n",
          [Same,get_information(State)]),
        finish_test(State);
      _ ->
        case Index of
          0 -> {noreply,State};
          _ -> {noreply,State,?reserve_cycle}
        end
    end;

handle_cast(_X,State) ->
    ?dbg("handle_cast ~p~n",[_X]),
    #test_state{index=Index} = State,
    case Index of
      0 -> {noreply,State};
      _ -> {noreply,State,?reserve_cycle}
    end.

handle_info(timeout=_X,#test_state{index = 0}=State) ->
    ?dbg("handle_info ~p~n",[_X]),
    {noreply,State};

handle_info(timeout=_X,State) ->
    ?dbg("handle_info ~p~n",[_X]),
    test_reserve(State);

handle_info(_X,State) ->
    ?dbg("handle_info ~p~n",[_X]),
    #test_state{index=Index} = State,
    case Index of
      0 -> {noreply,State};
      _ -> {noreply,State,?reserve_cycle}
    end.

terminate(_Reason,State) ->
    ?dbg("terminate ~p~n",[_Reason]),
    #test_state{table=Tbl} = State,
    ets:delete(Tbl),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok,State}.


test_reserve(State) ->
    #test_state{table=Tbl,index=Index} = State,
    [{_,Ranges}] = ets:lookup(Tbl,ranges),
    Now = os:timestamp(),
    case catch allocator:reserve(Ranges) of
      {ok,Value} ->
        case timer:now_diff(os:timestamp(),Now) of
          T when T < 1000 -> Counter = count_1ms;
          T when T < 2000 -> Counter = count_2ms;
          T when T < 3000 -> Counter = count_3ms;
          T when T < 4000 -> Counter = count_4ms;
          T when T < 5000 -> Counter = count_5ms;
          T when T < 6000 -> Counter = count_6ms;
          T when T < 7000 -> Counter = count_7ms;
          T when T < 8000 -> Counter = count_8ms;
          T when T < 9000 -> Counter = count_9ms;
          T when T < 10000 -> Counter = count_10ms;
          T when T >= 10000 -> Counter = count_over
        end,
        case which_range(Ranges,Value) of
          primary -> I = 2;
          secondary -> I = 3 end,
        ets:update_counter(Tbl,Counter,{I,1}),
        ets:update_counter(Tbl,count_reserved,{2,1}),
        Data = [ {origin,{?test_srv,node()}}
               , {value,Value}
               , {index,Index}
               , {action,{?MODULE,test_callback}} ],
        allocator:write(Value,Data,?cache_time_out),
        case Index - 1 of
          N when 0 < N ->
            {noreply,State#test_state{index=N},?reserve_cycle};
          _ ->
            io:format("~nTest: all values reserved~n~n",[]),
            ets:insert(Tbl,{peak,allocator:info()}),
            {noreply,State#test_state{index=0}}
        end;
      Err ->
        {stop,{failed_at,Index,with_error,Err}}
    end.

which_range([{Name,Lo,Hi}|Rest],Value) ->
    if
      Lo =< Value, Value < Hi -> Name;
      true -> which_range(Rest,Value)
    end.

size_of_ranges([]) -> 0;
size_of_ranges([{_Name,Min,Max}|Rest]) -> Max - Min + size_of_ranges(Rest).

finish_test(State) ->
    #test_state{table=Tbl} = State,
    ets:insert(Tbl,{final,allocator:info()}),
    ets:insert(Tbl,{finish_time,os:timestamp()}),
    Count_values = get_information(State),
    Keys = [initial,peak,final],
    Values = [ X || K <- Keys, X <- ets:lookup(Tbl,K) ],
    [Initial,_,Final] = [ proplists:get_value(K,Values) || K <- Keys ],
    io:format("Test Report:~n~p~n",
      [ [{test_passed,Initial == Final}|Values]
        ++ [{reserve_delay,primary,secondary}|Count_values] ] ),
    {ok,Hashes} = application:get_env(allocator,hash_to_node),
    Nodes = [ N || {_,N} <- Hashes],
    {_,[]} = rpc:multicall(Nodes,application,stop,[allocator]),
    {stop,normal,State}.

get_information(State) ->
    #test_state{table=Tbl,index=Index} = State,
    Fn = fun
      ({K,_}=T,O) when is_atom(K) -> [T|O] ;
      (_,O) -> O end,
    Count_values =
      [ X || {C,_,_} <- counter_records(), X <- ets:lookup(Tbl,C) ],
    Compute_info =
      [ {index,Index}
      , {cpu_utilisation,cpu_sup:util()}
      , {time_stamp,os:timestamp()} | memsup:get_system_memory_data() ],
    lists:sort(ets:foldl(Fn,Compute_info,Tbl))
     ++ [{counters,[{reserve_delay,primary,secondary}|Count_values]}].

