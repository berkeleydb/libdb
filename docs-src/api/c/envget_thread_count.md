---
title: "DB_ENV->get_thread_count()"
api-name: "DB_ENV->get_thread_count()"
source: docs/api_reference/C/envget_thread_count.html
---
## DB_ENV-\>get_thread_count()

``` c
#include <db.h>

int
DB_ENV->get_thread_count(DB_ENV *dbenv, u_int32_t *countp);  
```

The `DB_ENV->get_thread_count()` method returns the thread count as set by the <a href="envset_thread_count.md" class="xref" title="DB_ENV-&gt;set_thread_count()">DB_ENV-&gt;set_thread_count()</a> method.

The `DB_ENV->get_thread_count()` method may be called at any time during the life of the application.

The `DB_ENV->get_thread_count()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### countp

The `DB_ENV->get_thread_count()` method returns the thread count in **countp**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>, <a href="envset_thread_count.md" class="xref" title="DB_ENV-&gt;set_thread_count()">DB_ENV-&gt;set_thread_count()</a>
