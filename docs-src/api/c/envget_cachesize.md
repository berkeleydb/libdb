---
title: "DB_ENV->get_cachesize()"
api-name: "DB_ENV->get_cachesize()"
source: docs/api_reference/C/envget_cachesize.html
---
## DB_ENV-\>get_cachesize()

``` c
#include <db.h>

int
DB_ENV->get_cachesize(DB_ENV *dbenv,
    u_int32_t *gbytesp, u_int32_t *bytesp, int *ncachep);  
```

The `DB_ENV->get_cachesize()` method returns the current size and composition of the cache, as set using the <a href="envset_cachesize.md" class="xref" title="DB_ENV-&gt;set_cachesize()">DB_ENV-&gt;set_cachesize()</a> method.

The `DB_ENV->get_cachesize()` method may be called at any time during the life of the application.

The `DB_ENV->get_cachesize()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### gbytesp

The **gbytesp** parameter references memory into which the gigabytes of memory in the cache is copied.

#### bytesp

The **bytesp** parameter references memory into which the additional bytes of memory in the cache is copied.

#### ncachep

The **ncachep** parameter references memory into which the number of caches is copied.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>, <a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>, <a href="envset_cachesize.md" class="xref" title="DB_ENV-&gt;set_cachesize()">DB_ENV-&gt;set_cachesize()</a>
