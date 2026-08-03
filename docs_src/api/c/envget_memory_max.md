---
title: "DB_ENV->get_memory_max()"
api-name: "DB_ENV->get_memory_max()"
source: docs/api_reference/C/envget_memory_max.html
---
## DB_ENV-\>get_memory_max()

``` c
#include <db.h>

int
DB_ENV->get_memory_max(DB_ENV *dbenv, u_int32_t *gbytesp, 
                       u_int32_t *bytesp); 
```

The `DB_ENV->get_memory_max()` method returns the maximum amount of memory to be used by shared structures other than mutexes and the page cache (memory pool). This value is set using the <a href="envset_memory_max.md" class="xref" title="DB_ENV-&gt;set_memory_max()">DB_ENV-&gt;set_memory_max()</a> method.

The `DB_ENV->get_memory_max()` method may be called at any time during the life of the application.

The `DB_ENV->get_memory_max()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### gbytesp

The **gbytesp** parameter references memory into which is copied the maximum number of gigabytes of memory that can be allocated.

#### bytesp

The **bytesp** parameter references memory into which is copied the additional bytes of memory that can be allocated.

#### sizep

The **sizep** parameter references memory into which is copied the maximum number of bytes to be allocated.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
