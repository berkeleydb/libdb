---
title: "DB_ENV->get_mp_mtxcount()"
api-name: "DB_ENV->get_mp_mtxcount()"
source: docs/api_reference/C/envget_mp_mtxcount.html
---
## DB_ENV-\>get_mp_mtxcount()

``` c
#include <db.h>

int
DB_ENV->get_mp_mtxcount(DB_ENV *dbenv, u_int32_t *mtxcount);  
```

The `DB_ENV->get_mp_mtxcount()` method returns the number of mutexes allocated for the hash table in the buffer pool.

### Parameters

#### mtxcount

This parameter specifies the number of mutexes allocated for the hash table in the buffer pool.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>, <a href="envset_mp_mtxcount.md" class="xref" title="DB_ENV-&gt;set_mp_mtxcount()">DB_ENV-&gt;set_mp_mtxcount()</a>
