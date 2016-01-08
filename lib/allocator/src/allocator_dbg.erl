-module(allocator_dbg).

-export([dbg_format/4]).

dbg_format(Mod,Line,Fmt,Arg) ->
    Pre = io_lib:format("~p.erl:~p: ~p ~p ~s~n\t",
      [Mod,Line,erlang:self(),erlang:node(),iso8601_now()]),
    F = "~s" ++ dbg_indent(Fmt),
    A = [Pre|Arg],
    io:format(F,A),
    true.

dbg_indent([]) -> [];
dbg_indent([$~,$n]) -> [$~,$n];
dbg_indent([$~,$n|Rest]) -> [$~,$n,$\t|dbg_indent(Rest)];
dbg_indent([Each|Rest]) -> [Each|dbg_indent(Rest)].

iso8601_now() ->
    Now = os:timestamp(),
    UTC = calendar:now_to_universal_time(Now),
    iso8601(UTC,Now).

iso8601({{Yr,Mo,Dy},{Hr,Mn,Sc}},{_,_,Mu}) ->
    [pad(4,Yr),$-,
     pad(2,Mo),$-,
     pad(2,Dy),$T,
     pad(2,Hr),$:,
     pad(2,Mn),$:,
     pad(2,Sc),$.,
     pad(6,Mu)].

pad(W,N) when is_integer(W), is_integer(N) ->
    L = erlang:integer_to_list(N),
    pad_each(W - length(L),L).

pad_each(W,L) when W > 0 -> [$0|pad_each(W-1,L)];
pad_each(_,L) -> L.
