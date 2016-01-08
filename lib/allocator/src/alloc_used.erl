-module(alloc_used).

-behaviour(gen_server).

-export([start_link/0,stop/0,subordinate/1]).

-export([lookup/1]).

-export( [  init/1 ,
     handle_call/3 ,
     handle_cast/2 ,
     handle_info/2 ,
       terminate/2 ,
     code_change/3 ] ).

-include("allocator_dbg.hrl").

-include("allocator.hrl").

-record(alloc_used_state,{ index_tbl, event_tbl, timer_ref }).


lookup(Index) ->
    case ets:lookup(?index_tbl,Index) of
      [#xref{value = Event}] ->
        [#future_event{data = Data}] = ets:lookup(?event_tbl,Event),
        Data;
      _ ->
        []
    end.


start_link() ->
    gen_server:start_link(
      {local,?used_srv},?MODULE,[],[]).

stop() ->
    gen_server:cast(?used_srv,stop).

subordinate(Args) ->
    Expand = [ proplists:lookup(K,Args) || K <- proplists:get_keys(Args) ],
    gen_server:start_link({local,?used_srv},?MODULE,Expand,[]).



init(Args) ->
    allocator:set_debug_flag(proplists:get_value(debug_on_for_seconds,Args)),
    case proplists:get_value(group_leader,Args) of
      GL when is_pid(GL) -> erlang:group_leader(GL,erlang:self());
      _ -> ok end,
    erlang:self() ! {?get_siblings,Args},
    {ok,#alloc_used_state{}}.


handle_call(_Msg,_,#alloc_used_state{index_tbl=undefined}=State) ->
    {reply,{error,no_index_tbl},State};
handle_call(_Msg,_,#alloc_used_state{event_tbl=undefined}=State) ->
    {reply,{error,no_event_tbl},State};

handle_call({?write_event,Index,Data,Expiry}=_Msg,_From,State) ->
    ?dbg("handle_call ~p~n",[_Msg]),
    Result = write_event(Index,Data,Expiry),
    {reply,Result,sweep_expired(State)};

handle_call({?update_event,Index,Data,Expiry}=_Msg,_From,State) ->
    ?dbg("handle_call ~p~n",[_Msg]),
    Result = update_event(Index,Data,Expiry),
    {reply,Result,sweep_expired(State)};

handle_call({?cancel_event,Index}=_Msg,_From,State) ->
    ?dbg("handle_call ~p~n",[_Msg]),
    Result = cancel_event(Index),
    {reply,Result,sweep_expired(State)};

handle_call(_Msg,_,State) ->
    ?dbg("handle_call ~p~n",[_Msg]),
    {reply,{error,not_recognised},sweep_expired(State)}.

handle_cast(stop,State) ->
    {stop,normal,State};

handle_cast({?debug_flag,Timeout}=_Msg,State) ->
    allocator:set_debug_flag(Timeout),
    ?dbg("handle_cast ~p~n",[_Msg]),
    {noreply,sweep_expired(State)};

handle_cast(_X,#alloc_used_state{index_tbl=undefined}=State) ->
    {noreply,State};
handle_cast(_X,#alloc_used_state{event_tbl=undefined}=State) ->
    {noreply,State};

handle_cast(_X,State) ->
    ?dbg("handle_cast ~p~n",[_X]),
    {noreply,sweep_expired(State)}.


handle_info({?get_siblings,Args}=_X,State) ->
    ?dbg("handle_info ~p~n",[_X]),
    Supervisor = proplists:get_value(supervisor,Args),
    case is_alive(Supervisor) of
      true ->
        Children = supervisor:which_children(Supervisor),
        Siblings = [ Pid || {_,Pid,_,[Mod]} <- Children, Mod /= ?MODULE ],
        ?dbg("Siblings ~p~n",[Siblings]),
        [ P ! {?waiting_for_tables,?MODULE,erlang:self()} || P <- Siblings ],
        {noreply,sweep_expired(State)};
      _ -> {stop,dead_supervisor}
    end;


handle_info({'ETS-TRANSFER',?event_tbl,_Ctrl,restore_event_tbl}=_Msg,State) ->
    ?dbg("handle_info ~p~n",[_Msg]),
    {noreply,sweep_expired(State#alloc_used_state{event_tbl=?event_tbl})};
handle_info({'ETS-TRANSFER',?index_tbl,_Ctrl,restore_index_tbl}=_Msg,State) ->
    ?dbg("handle_info ~p~n",[_Msg]),
    {noreply,sweep_expired(State#alloc_used_state{index_tbl=?index_tbl})};
handle_info(_X,#alloc_used_state{index_tbl=undefined}=State) ->
    {noreply,State};
handle_info(_X,#alloc_used_state{event_tbl=undefined}=State) ->
    {noreply,State};

handle_info(_X,State) ->
    ?dbg("handle_info ~p~n",[_X]),
    {noreply,sweep_expired(State)}.

terminate(_Reason, #alloc_used_state{timer_ref = Last}) ->
    ?dbg("terminate ~p~n",[_Reason]),
    clear_timer(Last),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok,sweep_expired(State)}.



update_event(Index,New_data,Expiry) ->
    ?dbg("update_event ~p,~p,~p~n",[Index,New_data,Expiry]),
    try
      Old_data = delete(Index),
      Data = deduplicate(New_data ++ [{last_update,os:timestamp()}|Old_data]),
      Event = #future_event{ expiry = {Expiry,Index}, data = Data },
      Xref = #xref{ index = Index, value = {Expiry,Index} },
      true = ets:insert_new(?event_tbl,Event),
      true = ets:insert_new(?index_tbl,Xref),
      {ok,Old_data}
    catch Error:Reason ->
      {Error,Reason}
    end.

write_event(Index,Data,Expiry) ->
    ?dbg("write_event ~p,~p,~p~n",[Index,Data,Expiry]),
    try
      Old_data = delete(Index),
      Event = #future_event{ expiry = {Expiry,Index}, data = Data },
      Xref = #xref{ index = Index, value = {Expiry,Index} },
      true = ets:insert_new(?event_tbl,Event),
      true = ets:insert_new(?index_tbl,Xref),
      {ok,Old_data}
    catch Error:Reason ->
      {Error,Reason}
    end.

cancel_event(Index) ->
    ?dbg("cancel_event ~p~n",[Index]),
    try
      Old_data = delete(Index),
      {ok,Old_data}
    catch Error:Reason ->
      {Error,Reason}
    end.

delete(Index) ->
    case ets:lookup(?index_tbl,Index)
      of [#xref{value = Key}] ->
           [#future_event{data = Old_data}] = ets:lookup(?event_tbl,Key),
           true = ets:delete(?index_tbl,Index),
           true = ets:delete(?event_tbl,Key),
           Old_data;
         [] ->
           []
     end.


deduplicate(List) ->
    dedup_each(proplists:get_keys(List),List).

dedup_each([],_) -> [];
dedup_each([Key|Rest],List) ->
    case proplists:lookup(Key,List) of
      {_,undefined} -> dedup_each(Rest,List);
      {Tag,true} when is_atom(Tag) -> [Tag|dedup_each(Rest,List)];
      Each when is_tuple(Each) -> [Each|dedup_each(Rest,List)];
      _ -> dedup_each(Rest,List)
    end.

sweep_expired(#alloc_used_state{event_tbl=undefined}=State) -> State;
sweep_expired(#alloc_used_state{index_tbl=undefined}=State) -> State;
sweep_expired(#alloc_used_state{timer_ref = Last}=State) ->
    ?dbg("sweep_expired~nState=~p~n",[State]),
    clear_timer(Last),
    Now = os:timestamp(),
    Next = each_expired(Now),
    State#alloc_used_state{timer_ref = Next}.

each_expired(Now) ->
    ?dbg("each_expired~n",[]),
    case ets:first(?event_tbl)
      of '$end_of_table' ->
           undefined;
         {Expiry,Index} = Key when Expiry < Now ->
           case ets:lookup(?event_tbl,Key)
             of [#future_event{data=Data}] -> process_event(Data);
                _ -> io:format("~p: Index=~p is gone!~n",[?LINE,Index])
            end,
           true = ets:delete(?index_tbl,Index),
           true = ets:delete(?event_tbl,Key),
           each_expired(Now);
         {Expiry,_} ->
           set_timer(time_diff(Expiry,Now))
     end.

time_diff(A,B) ->
    timer:now_diff(A,B) div 1000.


process_event(Data) ->
    ?dbg("process_event Data=~p~n",[Data]),
    try
      case proplists:get_value(action,Data) of
        {Mod,Fun} when is_atom(Mod), is_atom(Fun) ->
          _Result = erlang:apply(Mod,Fun,[Data]),
          ?dbg("erlang:apply(~p,~p,~p) -> ~p~n",[Mod,Fun,Data,_Result]);
        Fun when is_function(Fun,1) ->
          _Result = Fun(Data),
          ?dbg("~p() -> ~p~n",[Fun,_Result]);
        _ ->
          ok
      end
    catch _Error:_Reason ->
      ?dbg("Action failed with ~p:~p~n",[_Error,_Reason])
    end.



-define(MIN_PERIOD,0). % margin to ensure that timers are in the future.

set_timer(Period) when Period < ?MIN_PERIOD ->
    set_timer(?MIN_PERIOD);
set_timer(Period) ->
    erlang:send_after(Period,erlang:self(),timer_event).

clear_timer(R) when is_reference(R) ->
    _ = erlang:cancel_timer(R),
    ok;
clear_timer(_) ->
    ok.

is_alive(Srv) when is_pid(Srv) -> erlang:is_process_alive(Srv);
is_alive(undefined) -> false;
is_alive(Name) -> is_alive(erlang:whereis(Name)).
