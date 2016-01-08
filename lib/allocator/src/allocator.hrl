-define(debug_flag,debug_active_expiry).

-define(event_tbl,alloc_used_event_tbl).
-define(free_srv,alloc_free_gen_srv).
-define(free_tbl,allocator_free_tbl).
-define(get_siblings,get_siblings).
-define(hash_tbl,allocator_hash_tbl).
-define(index_tbl,alloc_used_index_tbl).
-define(req_release_hop,req_release_hop).
-define(request_reserve,request_reserve).
-define(used_srv,alloc_used_gen_srv).
-define(write_event,write_event).
-define(update_event,update_event).
-define(cancel_event,cancel_event).

-define(waiting_for_tables,waiting_for_tables).

-define(default_quarantine_period,900). %%  900 seconds == 15 minutes

-define(restore_free_tbl,restore_free_tbl).
-define(restore_index_tbl,restore_index_tbl).
-define(restore_event_tbl,restore_event_tbl).

-record(future_event,{ expiry, data }).
-record(xref,{ index, value }).
