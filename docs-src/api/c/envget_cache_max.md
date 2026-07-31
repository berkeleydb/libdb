---
title: "DB_ENV->get_cache_max()"
api-name: "DB_ENV->get_cache_max()"
source: docs/api_reference/C/envget_cache_max.html
---
## DB_ENV-\>get_cache_max()

``` c
#include <db.h>

int
DB_ENV->get_cache_max(DB_ENV *dbenv, u_int32_t *gbytesp, 
                      u_int32_t *bytesp);  
```

The `DB_ENV->get_cache_max()` method returns the maximum size of the cache as set using the <a href="envset_cache_max.md" class="xref" title="DB_ENV-&gt;set_cache_max()">DB_ENV-&gt;set_cache_max()</a> method.

The `DB_ENV->get_cache_max()` method may be called at any time during the life of the application.

The `DB_ENV->get_cache_max()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### gbytesp

The **gbytesp** parameter references memory into which the gigabytes of memory in the cache is copied.

#### bytesp

The **bytesp** parameter references memory into which the additional bytes of memory in the cache is copied.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>, <a href="envset_cache_max.md" class="xref" title="DB_ENV-&gt;set_cache_max()">DB_ENV-&gt;set_cache_max()</a>
