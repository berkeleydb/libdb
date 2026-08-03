---
title: "DB_ENV->set_mp_mtxcount()"
api-name: "DB_ENV->set_mp_mtxcount()"
source: docs/api_reference/C/envset_mp_mtxcount.html
---
## DB_ENV-\>set_mp_mtxcount()

``` c
#include <db.h>

int
DB_ENV->set_mp_mtxcount(DB_ENV *dbenv, u_int32_t mtxcount);  
```

The `DB_ENV->set_mp_mtxcount()` method overrides the default number of mutexes for the hash table in each memory pool cache. The defualt is one mutex per hash bucket. Setting it to a lower number decreases the number of mutexes used and the amount of memory needed to store them at the expense of concurrency in the memory pool. This can also improve startup time. Setting a number greater than the number size of the hash table will waste mutexes and space.

You must call this method only before the environment is opened.

### Parameters

#### mtxcount

Specifies the number of mutexes allocated to the buffer pool hash table.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>, <a href="envget_mp_mtxcount.md" class="xref" title="DB_ENV-&gt;get_mp_mtxcount()">DB_ENV-&gt;get_mp_mtxcount()</a>
