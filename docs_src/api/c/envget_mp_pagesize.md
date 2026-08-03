---
title: "DB_ENV->get_mp_pagesize()"
api-name: "DB_ENV->get_mp_pagesize()"
source: docs/api_reference/C/envget_mp_pagesize.html
---
## DB_ENV-\>get_mp_pagesize()

``` c
#include <db.h>

int
DB_ENV->get_mp_pagesize(DB_ENV *dbenv, u_int32_t *pagesizep);  
```

The `DB_ENV->get_mp_pagesize()` method returns the assumed page size used to configure the buffer pool.

The `DB_ENV->get_mp_pagesize()` method may be called at any time during the life of the application.

### Parameters

#### pagesizep

This parameter specifies the assumed page size used to configure the buffer pool.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>, <a href="envset_mp_pagesize.md" class="xref" title="DB_ENV-&gt;set_mp_pagesize()">DB_ENV-&gt;set_mp_pagesize()</a>
