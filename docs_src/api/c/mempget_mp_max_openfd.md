---
title: "DB_ENV->get_mp_max_openfd()"
api-name: "DB_ENV->get_mp_max_openfd()"
source: docs/api_reference/C/mempget_mp_max_openfd.html
---
## DB_ENV-\>get_mp_max_openfd()

``` c
#include <db.h>

int
DB_ENV->get_mp_max_openfd(DB_ENV *env, int *maxopenfdp);  
```

Returns the maximum number of file descriptors the library will open concurrently when flushing dirty pages from the cache. This value is set by the <a href="mempset_mp_max_openfd.md" class="xref" title="DB_ENV-&gt;set_mp_max_openfd()">DB_ENV-&gt;set_mp_max_openfd()</a> method.

The `DB_ENV->get_mp_max_openfd()` method may be called at any time during the life of the application.

The `DB_ENV->get_mp_max_openfd()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### maxopenfdp

The `DB_ENV->get_mp_max_openfd()` method returns the maximum number of file descriptors open in **maxopenfdp**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>, <a href="mempset_mp_max_openfd.md" class="xref" title="DB_ENV-&gt;set_mp_max_openfd()">DB_ENV-&gt;set_mp_max_openfd()</a>
