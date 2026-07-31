---
title: "DB_ENV->get_lk_max_lockers()"
api-name: "DB_ENV->get_lk_max_lockers()"
source: docs/api_reference/C/envget_lk_max_lockers.html
---
## DB_ENV-\>get_lk_max_lockers()

``` c
#include <db.h>

int
DB_ENV->get_lk_max_lockers(DB_ENV *dbenv, u_int32_t *lk_maxp);  
```

The `DB_ENV->get_lk_max_lockers()` method returns the maximum number of potential lockers. You can configure this using the <a href="envset_lk_max_lockers.md" class="xref" title="DB_ENV-&gt;set_lk_max_lockers()">DB_ENV-&gt;set_lk_max_lockers()</a> method.

The `DB_ENV->get_lk_max_lockers()` method may be called at any time during the life of the application.

The `DB_ENV->get_lk_max_lockers()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### lk_maxp

The `DB_ENV->get_lk_max_lockers()` method returns the maximum number of lockers in **lk_maxp**.

### Errors

The `DB_ENV->get_lk_max_lockers()` method may fail and return one of the following non-zero errors:

#### EINVAL

The method was called on an environment which had been opened without being configured for locking.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="lock.md" class="link" title="Chapter 6.  The DB_LOCK Handle">DB_LOCK</a>

### See Also

<a href="lock.md#locklist" class="xref" title="Locking Subsystem and Related Methods">Locking Subsystem and Related Methods</a>, <a href="envset_lk_max_lockers.md" class="xref" title="DB_ENV-&gt;set_lk_max_lockers()">DB_ENV-&gt;set_lk_max_lockers()</a>
