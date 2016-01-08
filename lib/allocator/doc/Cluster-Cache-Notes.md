
# Application `allocator`

## Overview

Implementation in the following applications:

 * `allocator`

### Application supervision heirarchy

```
┏━━━━━━━━━━━━━━━━━━━━━━━━┓
┃ module: allocator_app  ┃
┃ behaviour: application ┃
┗━┯━━━━━━━━━━━━━━━━━━━━━━┛
  │
  │  ┏━━━━━━━━━━━━━━━━━━━━━━━┓
  └──┨ module: allocator_sup ┃
     ┃ behaviour: supervisor ┃
     ┃ not registered        ┃
     ┗━┯━━━━━━━━━━━━━━━━━━━━━┛
       │
       │ one-for-one, permanent.
       │
       │  ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
       ├──┨ module: allocator              ┃  Manages the ETS tables of
       │  ┃ behaviour: gen_server          ┃  the alloc_free and the
       │  ┃ registered: allocator_gen_srv  ┃  alloc_used servers;
       │  ┃ functions:                     ┃
       │  ┃   reserve/1,release/1,         ┃  Exposes the API of the
       │  ┃   write/3,update/3,            ┃  allocator application,
       │  ┃   lookup/1,cancel/1,           ┃  implemented in the
       │  ┃   info/0,mem_info/0,           ┃  alloc_free and the 
       │  ┃   debug/1,to_intern_fmt/1      ┃  alloc_used servers.
       │  ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
       │
       │  ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
       ├──┨ module: alloc_free             ┃  Updates the store of
       │  ┃ behaviour: gen_server          ┃  released allocations
       │  ┃ registered: alloc_free_gen_srv ┃  in the allocator_free_tbl
       │  ┃ no functions, API in the       ┃  ETS table.
       │  ┃   allocator module only.       ┃
       │  ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
       │
       │  ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
       └──┨ module: alloc_used             ┃  Updates and manages the
          ┃ behaviour: gen_server          ┃  reserved allocations
          ┃ registered: alloc_used_gen_srv ┃  stored in the ETS tables
          ┃ no functions, API in the       ┃  alloc_used_event_tbl and
          ┃   allocator module only.       ┃  alloc_used_index_tbl.
          ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
```



### Support modules

In addition to the core application modules:

 * `allocator_dbg` for writing to the console (to be
   enhanced for use with a report handler);

 * `allocator_test` for unit-testing the application.


### Design overview

The `allocator` application is designed to operate as a
cluster of nodes, each running the application and
maintaining a fraction of the overall range of values to be
reserved and released.  For example, 32 nodes will each
manage 1/32th of the entire set of available values.

The nodes may be launched all colocated on a single machine
for testing and small scale deployment, or spread among
several machines, to enable easy capacity scaling. 
Increasing storage and processing capacity involves
spreading the Erlang nodes over a larger cluster of
machines.

Each reserved value has data associated with it, and may
have more than one lookup key to retrieve the data, or to
update the contents of the data.

Each value is reserved for a limited period of time, after
which the reservation lapses, and the allocated value is
returned to the pool of released values.  The reservation
may be refreshed and prolonged by updating the associated
data stored with the reserved value.



#### Configuration

The application environment may specify the following parameters:

 * `debug_on_for_seconds` enable debug tracing, active for
   a limited time in seconds, after which debugging ceases
   (requires that the `allocator` is compiled with the
   `-DDEBUG` flag, thus: `make ERLC_FLAGS=-DDEBUG allocator.app`);

 * `ranges` the ranges of valid values that may be reserved
   and released.  The ranges are numeric, with support for
   values of IPv4 and IPv6 address ranges and subnets;

 * `restore_all_free` boolean flag. Starting the
   application may either populate the store with released
   allocations if set to `true`, or set all ranges as
   reserved if set `false`. Default is `false`;

 * `hash_to_node` mapping of a hashed number to an Erlang
   node name. The list contains all the nodes participating
   in a cluster.


#### Ranges

Reserving an allocation may specify one or more `primary`
ranges of preferred values, and zero or more `secondary`
ranges of fall-back values if values in the `primary`
ranges are no longer available.

The test results shows that a cluster of 32 nodes, with a
range covering 10000 values split into primary and
secondary ranges, will reserve the primary values in mostly
under 1ms while these are available.  When the primary
values are exhausted, the secondary values are reserved
within 10ms.


    5000 |
         | P
    4000 | P
         | P
    3000 | P
         | P                 S
    2000 | P                 S
         | P                 S  S
    1000 | P                 S  S
         | P           S  S  S  S
       0 | P  P  S  S  S  S  S  S  S  S  S
         +--------------------------------
           0  1  2  3  4  5  6  7  8  9 10


When primary values become increasingly rare, the search
traverses a growing proportion of the nodes in the cluster,
which explains the second peak in the graph at roughly 6ms
latency in reserving values.

#### Range configuration format

The supported format of values in the `range` configuration
parameter can either be numeric, or be IPv4 or IPv6 address
ranges. For example:

 * `{ranges,[{'Name-of-range',0,10000}]}` numeric values;

 * `{ranges,[{'TEST_1',"10.0.0.0","10.1.2.3"}]}` IPv4 range
   start and end values;

 * `{ranges,[{'TEST_2',"10.0.0.0/8"}]}` IPv4 subnet range,
   equivalent to `{'TEST_2,"10.0.0.0","11.0.0.0"'}` or to
   the numeric range 281470849515520 to 281470866292736
   (numeric values of the given IPv4 addresses mapped to IPv6);

 * `{ranges,[{'TEST-IPv6',"2001:db8::/114"}]}` IPv6 subnet
  range (see example below).

For example, to convert the configured IPv4 and IPv6 values
to numbers:

```erlang
1> allocator:to_intern_fmt([{ranges,[{'TEST-IPv6',"2001:db8::/114"}]}]).
[{ranges,[{'TEST-IPv6',42540766411282592856903984951653826560,
                       42540766411282592856903984951653842944}]}]

2> allocator:to_intern_fmt([{ranges,[{'TEST-IPv4',"10.0.0.0/8"}]}]).
[{ranges,[{'TEST-IPv4',281470849515520,281470866292736}]}]
```


#### Health monitoring


The `allocator` monitors all other nodes in the cluster. 
If a `'DOWN'` is received, then the `req_reserve_hop` will
skip the lost node when attempting to reserve an allocation
on the next node in the `hash_tbl`.


Processes recover from crashes by the OTP restart mechanism.
The ETS tables are managed such that the table contents are
not lost from a process crash.


#### Restarting a crashed node

A crashed node is conventionally restarted automatically,
and has to rejoin a cluster once it is ready for work.  The
restarted node must mark all the values in its fraction of
the whole range as being reserved with a default
reservation period.

The allocations may still be in use, and have to be
rediscovered in the traffic going through the entire
system, being fed into the `alloc_used` tables by calls to
refresh the data of the allocations still in use.

After the recovery default reservation period has lapsed,
those reservations that had not received any update will be
released and made available for new allocations.




## Testing methods

All testing relies on launching a cluster of Erlang node. 
These run on the local machine for convenience.  Memory
requirements may grow depending on the span of the ranges
used.  The default ranges used in `allocator_test` are
`10.0.0.0/16`, i.e.  65536 values.

Script to start a cluster of nodes:

```bash
#!/bin/bash

Prefix="Nodes =
[ " ;
for Each in node_{00..31} ;
do echo -n "$Prefix$Each@$(hostname)" ;
   Prefix=",
  " ;
   erl -pa ./lib/*/ebin \
       -setcookie cluster \
       -sname "$Each" \
       -noinput \
       -hidden \
       -detached ;
done ;
echo " ]."
```

The script produces an Erlang list of the started nodes, to
be reused in the Erlang shell:

```erlang
Nodes =
[ 'node_00@Pong', 'node_01@Pong', 'node_02@Pong', 'node_03@Pong',
  'node_04@Pong', 'node_05@Pong', 'node_06@Pong', 'node_07@Pong',
  'node_08@Pong', 'node_09@Pong', 'node_10@Pong', 'node_11@Pong',
  'node_12@Pong', 'node_13@Pong', 'node_14@Pong', 'node_15@Pong',
  'node_16@Pong', 'node_17@Pong', 'node_18@Pong', 'node_19@Pong',
  'node_20@Pong', 'node_21@Pong', 'node_22@Pong', 'node_23@Pong',
  'node_24@Pong', 'node_25@Pong', 'node_26@Pong', 'node_27@Pong',
  'node_28@Pong', 'node_29@Pong', 'node_30@Pong', 'node_31@Pong' ].
```

The test options are:

 * [Manual testing][] -- The [Basic start up][] is most useful;

 * [Automated testing][] -- Proper stuff.



### Manual testing

#### Basic start up

The manual test is quite basic, to veify that by default a
node starts with all allocations reserved in quarantine,
and after a configured period of time, the allocations are
released to the free set.

Start a single node:

```bash
erl -pa ./lib/*/ebin -sname node_00 -setcookie cluster
```


Start an `allocator` on the node, and check that there are
no free numbers:

```erlang
(node_00@Pong)1> application:start(allocator).
(node_00@Pong)2> allocator:info().
[{lost_nodes,[node_20@Pong,node_08@Pong,node_28@Pong,
              node_02@Pong,node_14@Pong,node_12@Pong,node_27@Pong,
              node_18@Pong,node_04@Pong,node_01@Pong,node_31@Pong,
              node_03@Pong,node_05@Pong,node_16@Pong,node_07@Pong,
              node_06@Pong,node_09@Pong,node_10@Pong,node_13@Pong,
              node_26@Pong,node_30@Pong,node_21@Pong,node_25@Pong,
              node_15@Pong,node_11@Pong,node_19@Pong|...]},
 {'TEST_1',[{base,281470849515520},{size,262144},{free,0}]}]
(node_00@Pong)3> allocator:mem_info().
[{allocator_hash_tbl,[{memory,616},{size,32}]},
 {allocator_free_tbl,[{memory,89},{size,0}]},
 {alloc_used_index_tbl,[{memory,123576},{size,8133}]},
 {alloc_used_event_tbl,[{memory,301010},{size,8133}]}]
```

The `mem_info/0` function reports the `size` of the index
and event tables as containing 8133 numbers.

After one minute, all 8133 numbers are available to be reserved:

```erlang
(node_00@Pong)4> allocator:info().
[{lost_nodes,[node_20@Pong,node_08@Pong,node_28@Pong,
              node_02@Pong,node_14@Pong,node_12@Pong,node_27@Pong,
              node_18@Pong,node_04@Pong,node_01@Pong,node_31@Pong,
              node_03@Pong,node_05@Pong,node_16@Pong,node_07@Pong,
              node_06@Pong,node_09@Pong,node_10@Pong,node_13@Pong,
              node_26@Pong,node_30@Pong,node_21@Pong,node_25@Pong,
              node_15@Pong,node_11@Pong,node_19@Pong|...]},
 {'TEST_1',[{base,281470849515520},
            {size,262144},
            {free,8133}]}]
(node_00@Pong)5> allocator:mem_info().
[{allocator_hash_tbl,[{memory,616},{size,32}]},
 {allocator_free_tbl,[{memory,65153},{size,8133}]},
 {alloc_used_index_tbl,[{memory,299},{size,0}]},
 {alloc_used_event_tbl,[{memory,89},{size,0}]}]
```


#### Test the cluster

This test operates on an entire cluster. It's more a
tyre-kicking exercise, only written here for reference. 
Connect to the Erlang nodes via another node:

```
./Cluster
erl -pa ./lib/*/ebin -sname phm$RANDOM -setcookie cluster 
```

Manually start the relevant applications on all the nodes,
after pasting the value of `Nodes` into the shell:

```erlang
{_,[]} = rpc:multicall(Nodes,application,start,[allocator]).
```

Setting all the nodes to print to the local node. The
Erlang script below will set the `group_leader` to the
local Erlang shell:

```erlang
GL = erlang:group_leader().
Pids = [ {N,rpc:call(N,erlang,whereis,[allocator_gen_srv])} || N <- Nodes ].
[ {down,N,P} || {N,P} <- Pids
              , true /= rpc:call(N,erlang,group_leader,[GL,P]) ].

rpc:call(node_12@Pong,io,format,["Hello~n",[]]).
```

Inspecting the contents of the ETS tables `allocator_free_tbl`:

```erlang
{X,[]} = rpc:multicall(Nodes,ets,info,[allocator_free_tbl]).
W = [ Y || Z <- X, {size,Y} <- Z].
10000 = lists:foldl(fun (In,Out) -> In + Out end,0,W).
```

The tests below assume a range defined from value 0 to 10000.

This one should fail:

```
{error,time_out} = allocator:reserve(
  {allocator_gen_srv,node_16@Pong},
  [{primary,20000,20001}]).
```

This should return successively increasing allocations:

```erlang
{ok,N} = allocator:reserve(
  {allocator_gen_srv,node_16@Pong},
  [{primary,5000,10000},{secondary,0,5000}]).

ok = allocator:release({allocator_gen_srv,node_00@Pong},N).

ok = allocator:release({allocator_gen_srv,node_01@Pong},N).

allocator:info().
```



### Automated testing

The automated test launches all the `allocator` servers in
the cluster with all the numbers in the range made
available, and then proceeds to reserve them all.  The
reserved numbers are set to release themselves at the end
of a limited time.  On completion, the test will report
either success or failure, along with a report containing
statistics from the test run.


Start the cluster of nodes with the `Cluster` script, and
remotely connect to the shell of one of the cluster nodes:

```
./Cluster
erl -pa ./lib/*/ebin -sname phm$RANDOM -setcookie cluster -remsh node_30@Pong
```

Start the test:

```erlang
(node_30@Pong)5> allocator_test:start_link().

Test: prepared to proceed with 65536 allocations.

{ok,<0.61.0>}
```

During the test run, the running statistics can be viewed:

```erlang
(node_30@Pong)6> rp(allocator_test:info()).
[{buffered_memory,1073152},
 {cached_memory,4120489984},
 {cpu_utilisation,40.63464081092992},
 {free_memory,9311977472},
 {free_swap,8589930496},
 {index,28684},
 {initial,[{lost_nodes,[]},
           {primary,[{base,281470857904128},{size,32768},{free,32768}]},
           {secondary,[{base,281470849515520},
                       {size,32768},
                       {free,32768}]}]},
 {ranges,[{primary,281470857904128,281470857936896},
          {secondary,281470849515520,281470849548288}]},
 {start_time,{1427,219137,590457}},
 {system_total_memory,16495611904},
 {time_stamp,{1427,219177,6193}},
 {total_memory,16495611904},
 {total_swap,8589930496},
 {counters,[{reserve_delay,primary,secondary},
            {count_1ms,32554,0},
            {count_2ms,104,0},
            {count_3ms,58,0},
            {count_4ms,31,8},
            {count_5ms,12,80},
            {count_6ms,7,1975},
            {count_7ms,1,1922},
            {count_8ms,1,69},
            {count_9ms,0,22},
            {count_10ms,0,7},
            {count_over,0,1},
            {count_reserved,36852,0}]}]
```


ETS table memory usage information:

```erlang
(node_30@Pong)7> allocator:mem_info().
[{allocator_hash_tbl,[{memory,523},{size,32}]},
 {allocator_free_tbl,[{memory,13521},{size,1679}]},
 {alloc_used_index_tbl,[{memory,6644},{size,423}]},
 {alloc_used_event_tbl,[{memory,17855},{size,423}]}]

(node_30@Pong)8> allocator:info().
[{lost_nodes,[]},
 {primary,[{base,281470857904128},{size,32768},{free,0}]},
 {secondary,[{base,281470849515520},
             {size,32768},
             {free,27055}]}]
```


Reaching halfway when all the values are reserved, and are
then being released once the reservation period expires for
each record:

```erlang

Test: all values reserved

(node_30@Pong)14> rp(allocator_test:info()).
[{buffered_memory,1073152},
 {cached_memory,4121194496},
 {cpu_utilisation,38.350910834132314},
 {free_memory,9227423744},
 {free_swap,8589930496},
 {index,0},
 {initial,[{lost_nodes,[]},
           {primary,[{base,281470857904128},{size,32768},{free,32768}]},
           {secondary,[{base,281470849515520},
                       {size,32768},
                       {free,32768}]}]},
 {peak,[{lost_nodes,[]},
        {primary,[{base,281470857904128},{size,32768},{free,0}]},
        {secondary,[{base,281470849515520},{size,32768},{free,0}]}]},
 {ranges,[{primary,281470857904128,281470857936896},
          {secondary,281470849515520,281470849548288}]},
 {start_time,{1427,219137,590457}},
 {system_total_memory,16495611904},
 {time_stamp,{1427,219352,655004}},
 {total_memory,16495611904},
 {total_swap,8589930496},
 {counters,[{reserve_delay,primary,secondary},
            {count_1ms,32554,0},
            {count_2ms,104,0},
            {count_3ms,58,1},
            {count_4ms,31,81},
            {count_5ms,12,1628},
            {count_6ms,7,21327},
            {count_7ms,1,9059},
            {count_8ms,1,384},
            {count_9ms,0,179},
            {count_10ms,0,66},
            {count_over,0,43},
            {count_reserved,65536,0}]}]
```


The test finishes with a report:


```erlang
Test Report:
[{test_passed,true},
 {initial,[{lost_nodes,[]},
           {primary,[{base,281470857904128},{size,32768},{free,32768}]},
           {secondary,[{base,281470849515520},{size,32768},{free,32768}]}]},
 {peak,[{lost_nodes,[]},
        {primary,[{base,281470857904128},{size,32768},{free,0}]},
        {secondary,[{base,281470849515520},{size,32768},{free,0}]}]},
 {final,[{lost_nodes,[]},
         {primary,[{base,281470857904128},{size,32768},{free,32768}]},
         {secondary,[{base,281470849515520},{size,32768},{free,32768}]}]},
 {reserve_delay,primary,secondary},
 {buffered_memory,1073152},
 {cached_memory,4128186368},
 {cpu_utilisation,100.0},
 {final,[{lost_nodes,[]},
         {primary,[{base,281470857904128},{size,32768},{free,32768}]},
         {secondary,[{base,281470849515520},{size,32768},{free,32768}]}]},
 {finish_time,{1427,219571,460682}},
 {free_memory,9130889216},
 {free_swap,8589930496},
 {index,0},
 {initial,[{lost_nodes,[]},
           {primary,[{base,281470857904128},{size,32768},{free,32768}]},
           {secondary,[{base,281470849515520},{size,32768},{free,32768}]}]},
 {peak,[{lost_nodes,[]},
        {primary,[{base,281470857904128},{size,32768},{free,0}]},
        {secondary,[{base,281470849515520},{size,32768},{free,0}]}]},
 {ranges,[{primary,281470857904128,281470857936896},
          {secondary,281470849515520,281470849548288}]},
 {start_time,{1427,219137,590457}},
 {system_total_memory,16495611904},
 {time_stamp,{1427,219571,461210}},
 {total_memory,16495611904},
 {total_swap,8589930496},
 {counters,[{reserve_delay,primary,secondary},
            {count_1ms,32554,0},
            {count_2ms,104,0},
            {count_3ms,58,1},
            {count_4ms,31,81},
            {count_5ms,12,1628},
            {count_6ms,7,21327},
            {count_7ms,1,9059},
            {count_8ms,1,384},
            {count_9ms,0,179},
            {count_10ms,0,66},
            {count_over,0,43},
            {count_reserved,65536,65536}]}]
```




Terminating all the nodes at the end of the test:

```erlang
Nodes =
[ 'node_00@Pong', 'node_01@Pong', 'node_02@Pong', 'node_03@Pong',
  'node_04@Pong', 'node_05@Pong', 'node_06@Pong', 'node_07@Pong',
  'node_08@Pong', 'node_09@Pong', 'node_10@Pong', 'node_11@Pong',
  'node_12@Pong', 'node_13@Pong', 'node_14@Pong', 'node_15@Pong',
  'node_16@Pong', 'node_17@Pong', 'node_18@Pong', 'node_19@Pong',
  'node_20@Pong', 'node_21@Pong', 'node_22@Pong', 'node_23@Pong',
  'node_24@Pong', 'node_25@Pong', 'node_26@Pong', 'node_27@Pong',
  'node_28@Pong', 'node_29@Pong', 'node_30@Pong', 'node_31@Pong' ].
{_,[]} = rpc:multicall(Nodes,init,stop,[]).
```













### Reloading modified modules into running nodes



```erlang
{_,[]} = rpc:multicall(Nodes,code,purge,[allocator]).
{_,[]} = rpc:multicall(Nodes,code,load_file,[allocator]).
```






## Appendix

### IPv6 address representation


```erlang
Ranges =
[ { ranges
  , [ {'IPv4 class A private subnet',"10.0.0.0/8"}
    , {'IPv4 class B private subnet',"172.16.0.0/12"}
    , {'IPv4 class C private subnet',"192.168.0.0/16"}
    , {'IPv4-mapped Address',"::ffff:0.0.0.0/96"}
    , {'Documentation',"2001:db8::/32"}
    , {'6to4',"2002::/16"}
    , {'Global Unicast',"2000::/3"} ] } ].

allocator:to_intern_fmt(Ranges).

[{ranges,
     [{'IPv4 class A private subnet',281470849515520,
          281470866292736},
      {'IPv4 class B private subnet',281473568473088,
          281473569521664},
      {'IPv4 class C private subnet',281473913978880,
          281473914044416},
      {'IPv4-mapped Address',281470681743360,281474976710656},
      {'Documentation',42540766411282592856903984951653826560,
          42540766490510755371168322545197776896},
      {'6to4',42545680458834377588178886921629466624,
          42550872755692912415807417417958686720},
      {'Global Unicast',42535295865117307932921825928971026432,
          85070591730234615865843651857942052864}]}]

Fn =
fun (Str)
    -> {ok,{A,B,C,D,E,F,G,H}} = inet:parse_ipv6_address(Str)
     , <<Int:128>> = <<A:16,B:16,C:16,D:16,E:16,F:16,G:16,H:16>>
     , Int end.

Addr =
[ "::ffff:0.0.0.0"
, "2001:db8::"
, "2002::"
, "2000::" ].

[{X,Fn(X)}||X<-Addr].

[{"::ffff:0.0.0.0",281470681743360},
 {"2001:db8::",42540766411282592856903984951653826560},
 {"2002::",42545680458834377588178886921629466624},
 {"2000::",42535295865117307932921825928971026432}]
```



### Box drawing


```
U+250x  ─       ━       │       ┃
        ┄       ┅       ┆       ┇
        ┈       ┉       ┊       ┋
        ┌       ┍       ┎       ┏
U+251x  ┐       ┑       ┒       ┓
        └       ┕       ┖       ┗
        ┘       ┙       ┚       ┛
        ├       ┝       ┞       ┟
U+252x  ┠       ┡       ┢       ┣
        ┤       ┥       ┦       ┧
        ┨       ┩       ┪       ┫
        ┬       ┭       ┮       ┯
U+253x  ┰       ┱       ┲       ┳
        ┴       ┵       ┶       ┷
        ┸       ┹       ┺       ┻
        ┼       ┽       ┾       ┿
U+254x  ╀       ╁       ╂       ╃
        ╄       ╅       ╆       ╇
        ╈       ╉       ╊       ╋
        ╌       ╍       ╎       ╏
U+255x  ═       ║
        ╒       ╓       ╔
        ╕       ╖       ╗
        ╘       ╙       ╚
        ╛       ╜       ╝
        ╞       ╟       ╠
        ╡       ╢       ╣
        ╤       ╥       ╦
        ╧       ╨       ╩
        ╪       ╫       ╬
        ╭       ╮       ╯       ╰
        ╱       ╲       ╳
        ╴       ╵       ╶       ╷
        ╸       ╹       ╺       ╻
        ╼       ╽       ╾       ╿
```
