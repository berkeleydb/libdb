---
title: "DB_ENV->set_mp_pagesize()"
api-name: "DB_ENV->set_mp_pagesize()"
source: docs/api_reference/C/envset_mp_pagesize.html
---
## DB_ENV-\>set_mp_pagesize()

``` c
#include <db.h>

int
DB_ENV->set_mp_pagesize(DB_ENV *dbenv, u_int32_t pagesize);  
```

The `DB_ENV->set_mp_pagesize()` method sets the pagesize used to allocate the hash table and the number of mutexes expected to be needed by the buffer pool.

This method may be called only before the environment is opened.

### Parameters

#### pagesize

The pagesize parameter specifies expected page size use. Generally, it is set to the expected average page size for all the data pages that are in the buffer pool.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>, <a href="envget_mp_pagesize.md" class="xref" title="DB_ENV-&gt;get_mp_pagesize()">DB_ENV-&gt;get_mp_pagesize()</a>
