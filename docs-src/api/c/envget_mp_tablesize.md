---
title: "DB_ENV->get_mp_tablesize()"
api-name: "DB_ENV->get_mp_tablesize()"
source: docs/api_reference/C/envget_mp_tablesize.html
---
## DB_ENV-\>get_mp_tablesize()

``` c
#include <db.h>

int
DB_ENV->get_mp_tablesize(DB_ENV *dbenv, u_int32_t *tablesizep);  
```

The `DB_ENV->get_mp_tablesize()` method returns the hash table size in the buffer pool.

### Parameters

#### tablesize

This parameter specifies the hash table size in the buffer pool.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>, <a href="envset_mp_tablesize.md" class="xref" title="DB_ENV-&gt;set_mp_tablesize()">DB_ENV-&gt;set_mp_tablesize()</a>
