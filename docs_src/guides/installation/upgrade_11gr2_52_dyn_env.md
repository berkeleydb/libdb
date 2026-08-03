---
title: "Dynamic Environment Configuration"
api-name: "Dynamic Environment Configuration"
source: docs/installation/upgrade_11gr2_52_dyn_env.html
---
## Dynamic Environment Configuration

<span class="sect2"> [New Functions](upgrade_11gr2_52_dyn_env.md#idp902144) </span>

<span class="sect2"> [Deprecated Functions](upgrade_11gr2_52_dyn_env.md#idp912000) </span>

Memory is now allocated incrementally as needed, instead of all at once during environment initialization, for structures that support locks, transactions, threads, and mutexes. With this change new functions have been added that configure how much memory is allocated initially, and how much that memory is allowed to grow. The old memory configuration functions have been deprecated.

### New Functions

- <a href="../../api/c/envset_memory_init.md" class="olink">DB_ENV-&gt;set_memory_init()</a>
- <a href="../../api/c/envget_memory_init.md" class="olink">DB_ENV-&gt;get_memory_init()</a>
- <a href="../../api/c/envset_memory_max.md" class="olink">DB_ENV-&gt;set_memory_max()</a>
- <a href="../../api/c/envget_memory_max.md" class="olink">DB_ENV-&gt;get_memory_max()</a>
- <a href="../../api/c/envset_lk_tablesize.md" class="olink">DB_ENV-&gt;set_lk_tablesize()</a>
- <a href="../../api/c/envget_lk_tablesize.md" class="olink">DB_ENV-&gt;get_lk_tablesize()</a>
- <a href="../../api/c/mutexset_init.md" class="olink">DB_ENV-&gt;mutex_set_init()</a>
- <a href="../../api/c/mutexget_init.md" class="olink">DB_ENV-&gt;mutex_get_init()</a>

### Deprecated Functions

- <a href="../../api/c/mutexset_max.md" class="olink">DB_ENV-&gt;mutex_set_max()</a>
- <a href="../../api/c/mutexget_max.md" class="olink">DB_ENV-&gt;mutex_get_max()</a>
- <a href="../../api/c/envset_lk_max_lockers.md" class="olink">DB_ENV-&gt;set_lk_max_lockers()</a>
- <a href="../../api/c/envget_lk_max_lockers.md" class="olink">DB_ENV-&gt;get_lk_max_lockers()</a>
- <a href="../../api/c/envset_lk_max_locks.md" class="olink">DB_ENV-&gt;set_lk_max_locks()</a>
- <a href="../../api/c/envget_lk_max_locks.md" class="olink">DB_ENV-&gt;get_lk_max_locks()</a>
- <a href="../../api/c/envset_lk_max_objects.md" class="olink">DB_ENV-&gt;set_lk_max_objects()</a>
- <a href="../../api/c/envget_lk_max_objects.md" class="olink">DB_ENV-&gt;get_lk_max_objects()</a>
- <a href="../../api/c/envset_thread_count.md" class="olink">DB_ENV-&gt;set_thread_count()</a>
- <a href="../../api/c/envget_thread_count.md" class="olink">DB_ENV-&gt;get_thread_count()</a>
- <a href="../../api/c/envset_tx_max.md" class="olink">DB_ENV-&gt;set_tx_max()</a>
- <a href="../../api/c/envget_tx_max.md" class="olink">DB_ENV-&gt;get_tx_max()</a>
