---
title: "Chapter 9.  Mutex Methods"
api-name: "Chapter 9.  Mutex Methods"
source: docs/api_reference/C/mutex.html
---
## Chapter 9.  Mutex Methods

This chapter describes methods that can be used to manage mutexes within DB. Many of the methods described here are used to configure DB's internal mutex system. However, a series of APIs are available for use as a general-purpose, cross platform mutex management system. These methods can be used independently of DB's main purpose, which is as a high-end data management engine.

## Mutex Methods

<table data-border="1" width="80%">
<thead>
<tr>
<th>Mutexes and Related Methods</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr>
<td><a href="mutexalloc.md" class="xref" title="DB_ENV-&gt;mutex_alloc()">DB_ENV-&gt;mutex_alloc()</a></td>
<td>Allocate a mutex</td>
</tr>
<tr>
<td><a href="mutexfree.md" class="xref" title="DB_ENV-&gt;mutex_free()">DB_ENV-&gt;mutex_free()</a></td>
<td>Free a mutex</td>
</tr>
<tr>
<td><a href="mutexlock.md" class="xref" title="DB_ENV-&gt;mutex_lock()">DB_ENV-&gt;mutex_lock()</a></td>
<td>Lock a mutex</td>
</tr>
<tr>
<td><a href="mutexstat.md" class="xref" title="DB_ENV-&gt;mutex_stat()">DB_ENV-&gt;mutex_stat()</a></td>
<td>Mutex statistics</td>
</tr>
<tr>
<td><a href="mutexstat_print.md" class="xref" title="DB_ENV-&gt;mutex_stat_print()">DB_ENV-&gt;mutex_stat_print()</a></td>
<td>Print mutex statistics</td>
</tr>
<tr>
<td><a href="mutexunlock.md" class="xref" title="DB_ENV-&gt;mutex_unlock()">DB_ENV-&gt;mutex_unlock()</a></td>
<td>Unlock a mutex</td>
</tr>
<tr>
<td colspan="2"><strong>Mutex Configuration</strong></td>
</tr>
<tr>
<td><a href="mutexset_align.md" class="xref" title="DB_ENV-&gt;mutex_set_align()">DB_ENV-&gt;mutex_set_align()</a>, <a href="mutexget_align.md" class="xref" title="DB_ENV-&gt;mutex_get_align()">DB_ENV-&gt;mutex_get_align()</a></td>
<td>Configure mutex alignment</td>
</tr>
<tr>
<td><a href="mutexset_increment.md" class="xref" title="DB_ENV-&gt;mutex_set_increment()">DB_ENV-&gt;mutex_set_increment()</a>, <a href="mutexget_increment.md" class="xref" title="DB_ENV-&gt;mutex_get_increment()">DB_ENV-&gt;mutex_get_increment()</a></td>
<td>Configure number of additional mutexes</td>
</tr>
<tr>
<td><a href="mutexset_init.md" class="xref" title="DB_ENV-&gt;mutex_set_init()">DB_ENV-&gt;mutex_set_init()</a>, <a href="mutexget_init.md" class="xref" title="DB_ENV-&gt;mutex_get_init()">DB_ENV-&gt;mutex_get_init()</a></td>
<td>Configure initial number of mutexes</td>
</tr>
<tr>
<td><a href="mutexset_max.md" class="xref" title="DB_ENV-&gt;mutex_set_max()">DB_ENV-&gt;mutex_set_max()</a>, <a href="mutexget_max.md" class="xref" title="DB_ENV-&gt;mutex_get_max()">DB_ENV-&gt;mutex_get_max()</a></td>
<td>Configure total number of mutexes</td>
</tr>
<tr>
<td><a href="mutexset_tas_spins.md" class="xref" title="DB_ENV-&gt;mutex_set_tas_spins()">DB_ENV-&gt;mutex_set_tas_spins()</a>, <a href="mutexget_tas_spins.md" class="xref" title="DB_ENV-&gt;mutex_get_tas_spins()">DB_ENV-&gt;mutex_get_tas_spins()</a></td>
<td>Configure test-and-set mutex spin count</td>
</tr>
</tbody>
</table>
