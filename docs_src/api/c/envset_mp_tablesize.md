---
title: "DB_ENV->set_mp_tablesize()"
api-name: "DB_ENV->set_mp_tablesize()"
source: docs/api_reference/C/envset_mp_tablesize.html
---
## DB_ENV-\>set_mp_tablesize()

``` c
#include <db.h>

int
DB_ENV->set_mp_tablesize(DB_ENV *dbenv, u_int32_t tablesize);  
```

The `DB_ENV->set_mp_tablesize()` method overrides the calculated hash table size. This value is then internally adjusted to a nearby prime number in order to enhance the hashing algorithm.

This method may be called only before the environment is opened.

### Parameters

#### tablesize

The table size parameter specifies the size of the buffer pool hash table. It is adjusted to a near prime number to enhance the hashing algorithm.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>, <a href="envget_mp_tablesize.md" class="xref" title="DB_ENV-&gt;get_mp_tablesize()">DB_ENV-&gt;get_mp_tablesize()</a>
