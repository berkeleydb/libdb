---
title: "DB_ENV->get_mp_max_write()"
api-name: "DB_ENV->get_mp_max_write()"
source: docs/api_reference/C/mempget_mp_max_write.html
---
## DB_ENV-\>get_mp_max_write()

``` c
#include <db.h>

int
DB_ENV->get_mp_max_write(DB_ENV *env, int *maxwritep, 
                         db_timeout_t *maxwrite_sleepp); 
```

The `DB_ENV->get_mp_max_write()` method returns the current maximum number of sequential write operations and microseconds to pause that the library can schedule when flushing dirty pages from the cache. These values are set by the <a href="mempset_mp_max_write.md" class="xref" title="DB_ENV-&gt;set_mp_max_write()">DB_ENV-&gt;set_mp_max_write()</a> method.

The `DB_ENV->get_mp_max_write()` method may be called at any time during the life of the application.

The `DB_ENV->get_mp_max_write()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### maxwritep

The **maxwritep** parameter references memory into which the maximum number of sequential write operations is copied.

#### maxwrite_sleepp

The **maxwrite_sleepp** parameter references memory into which the microseconds to pause before scheduling further write operations is copied.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>, <a href="mempset_mp_max_write.md" class="xref" title="DB_ENV-&gt;set_mp_max_write()">DB_ENV-&gt;set_mp_max_write()</a>
