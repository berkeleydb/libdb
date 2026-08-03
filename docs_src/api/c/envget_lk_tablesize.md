---
title: "DB_ENV->get_lk_tablesize()"
api-name: "DB_ENV->get_lk_tablesize()"
source: docs/api_reference/C/envget_lk_tablesize.html
---
## DB_ENV-\>get_lk_tablesize()

``` c
#include <db.h>

int
DB_ENV->get_lk_tablesize(DB_ENV *dbenv, u_int32_t *tablesizep; 
```

The `DB_ENV->get_lk_tablesize()` method returns the size of the lock object hash table in the Berkeley DB environment. This value is set using the <a href="envset_lk_tablesize.md" class="xref" title="DB_ENV-&gt;set_lk_tablesize()">DB_ENV-&gt;set_lk_tablesize()</a> method.

The `DB_ENV->get_lk_tablesize()` method may be called at any time during the life of the application.

The `DB_ENV->get_lk_tablesize()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### tablesizep

The **tablesizep** parameter references memory into which is copied the size of the lock object hash table configured for the Berkeley DB environment.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="lock.md" class="link" title="Chapter 6.  The DB_LOCK Handle">DB_LOCK</a>

### See Also

<a href="lock.md#locklist" class="xref" title="Locking Subsystem and Related Methods">Locking Subsystem and Related Methods</a>
