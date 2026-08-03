---
title: "Chapter 6.  The DB_LOCK Handle"
api-name: "Chapter 6.  The DB_LOCK Handle"
source: docs/api_reference/C/lock.html
---
## Chapter 6.  The DB_LOCK Handle

``` c
#include <db.h>

typedef struct __db_lock_u DB_LOCK;  
```

The locking interfaces for the Berkeley DB database environment are methods of the <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle. The `DB_LOCK` object is the handle for a single lock, and has no methods of its own.

## Locking Subsystem and Related Methods

<table data-border="1" width="80%">
<thead>
<tr>
<th>Locking Subsystem and Related Methods</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr>
<td><a href="lockdetect.md" class="xref" title="DB_ENV-&gt;lock_detect()">DB_ENV-&gt;lock_detect()</a></td>
<td>Perform deadlock detection</td>
</tr>
<tr>
<td><a href="lockget.md" class="xref" title="DB_ENV-&gt;lock_get()">DB_ENV-&gt;lock_get()</a></td>
<td>Acquire a lock</td>
</tr>
<tr>
<td><a href="lockid.md" class="xref" title="DB_ENV-&gt;lock_id()">DB_ENV-&gt;lock_id()</a></td>
<td>Acquire a locker ID</td>
</tr>
<tr>
<td><a href="lockid_free.md" class="xref" title="DB_ENV-&gt;lock_id_free()">DB_ENV-&gt;lock_id_free()</a></td>
<td>Release a locker ID</td>
</tr>
<tr>
<td><a href="lockput.md" class="xref" title="DB_ENV-&gt;lock_put()">DB_ENV-&gt;lock_put()</a></td>
<td>Release a lock</td>
</tr>
<tr>
<td><a href="lockstat.md" class="xref" title="DB_ENV-&gt;lock_stat()">DB_ENV-&gt;lock_stat()</a></td>
<td>Return lock subsystem statistics</td>
</tr>
<tr>
<td><a href="lockstat_print.md" class="xref" title="DB_ENV-&gt;lock_stat_print()">DB_ENV-&gt;lock_stat_print()</a></td>
<td>Print lock subsystem statistics</td>
</tr>
<tr>
<td><a href="lockvec.md" class="xref" title="DB_ENV-&gt;lock_vec()">DB_ENV-&gt;lock_vec()</a></td>
<td>Acquire/release locks</td>
</tr>
<tr>
<td><a href="envcdsgroup_begin.md" class="xref" title="DB_ENV-&gt;cdsgroup_begin()">DB_ENV-&gt;cdsgroup_begin()</a></td>
<td>Get a locker ID in Berkeley DB Concurrent Data Store</td>
</tr>
<tr>
<td colspan="2"><strong>Locking Subsystem Configuration</strong></td>
</tr>
<tr>
<td><a href="envset_timeout.md" class="xref" title="DB_ENV-&gt;set_timeout()">DB_ENV-&gt;set_timeout()</a>, <a href="envget_timeout.md" class="xref" title="DB_ENV-&gt;get_timeout()">DB_ENV-&gt;get_timeout()</a></td>
<td>Set/get lock and transaction timeout</td>
</tr>
<tr>
<td><a href="envset_lk_conflicts.md" class="xref" title="DB_ENV-&gt;set_lk_conflicts()">DB_ENV-&gt;set_lk_conflicts()</a>, <a href="envget_lk_conflicts.md" class="xref" title="DB_ENV-&gt;get_lk_conflicts()">DB_ENV-&gt;get_lk_conflicts()</a></td>
<td>Set/get lock conflicts matrix</td>
</tr>
<tr>
<td><a href="envset_lk_detect.md" class="xref" title="DB_ENV-&gt;set_lk_detect()">DB_ENV-&gt;set_lk_detect()</a>, <a href="envget_lk_detect.md" class="xref" title="DB_ENV-&gt;get_lk_detect()">DB_ENV-&gt;get_lk_detect()</a></td>
<td>Set/get automatic deadlock detection</td>
</tr>
<tr>
<td><a href="envset_lk_max_lockers.md" class="xref" title="DB_ENV-&gt;set_lk_max_lockers()">DB_ENV-&gt;set_lk_max_lockers()</a>, <a href="envget_lk_max_lockers.md" class="xref" title="DB_ENV-&gt;get_lk_max_lockers()">DB_ENV-&gt;get_lk_max_lockers()</a></td>
<td>Set/get maximum number of lockers</td>
</tr>
<tr>
<td><a href="envset_lk_max_locks.md" class="xref" title="DB_ENV-&gt;set_lk_max_locks()">DB_ENV-&gt;set_lk_max_locks()</a>, <a href="envget_lk_max_locks.md" class="xref" title="DB_ENV-&gt;get_lk_max_locks()">DB_ENV-&gt;get_lk_max_locks()</a></td>
<td>Set/get maximum number of locks</td>
</tr>
<tr>
<td><a href="envset_lk_max_objects.md" class="xref" title="DB_ENV-&gt;set_lk_max_objects()">DB_ENV-&gt;set_lk_max_objects()</a>, <a href="envget_lk_max_objects.md" class="xref" title="DB_ENV-&gt;get_lk_max_objects()">DB_ENV-&gt;get_lk_max_objects()</a></td>
<td>Set/get maximum number of lock objects</td>
</tr>
<tr>
<td><a href="envset_lk_partitions.md" class="xref" title="DB_ENV-&gt;set_lk_partitions()">DB_ENV-&gt;set_lk_partitions()</a>, <a href="envget_lk_partitions.md" class="xref" title="DB_ENV-&gt;get_lk_partitions()">DB_ENV-&gt;get_lk_partitions()</a></td>
<td>Set/get number of lock partitions</td>
</tr>
<tr>
<td><a href="envset_lk_priority.md" class="xref" title="DB_ENV-&gt;set_lk_priority()">DB_ENV-&gt;set_lk_priority()</a>, <a href="envget_lk_priority.md" class="xref" title="DB_ENV-&gt;get_lk_priority()">DB_ENV-&gt;get_lk_priority()</a></td>
<td>Set/get a locker's deadlock priority</td>
</tr>
<tr>
<td><a href="envset_lk_tablesize.md" class="xref" title="DB_ENV-&gt;set_lk_tablesize()">DB_ENV-&gt;set_lk_tablesize()</a>, <a href="envget_lk_tablesize.md" class="xref" title="DB_ENV-&gt;get_lk_tablesize()">DB_ENV-&gt;get_lk_tablesize()</a></td>
<td>Set/get size of the lock object hash table</td>
</tr>
</tbody>
</table>
