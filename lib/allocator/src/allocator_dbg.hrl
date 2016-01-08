-ifdef(DEBUG).
-define(dbg(F,A),
  ( erlang:get(?debug_flag) > os:timestamp() andalso
    allocator_dbg:dbg_format(?MODULE,?LINE,F,A) ) ).
-else.
-define(dbg(F,A),true).
-endif.

