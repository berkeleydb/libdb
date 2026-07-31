---
title: "DB_ENV->get_lk_priority()"
api-name: "DB_ENV->get_lk_priority()"
source: docs/api_reference/C/envget_lk_priority.html
---
## DB_ENV-\>get_lk_priority()

``` c
#include <db.h>

int
DB_ENV->get_lk_priority(DB_ENV *dbenv,
    u_int32_t lockerid, u_int32_t *priority);  
```

Get the deadlock priority for the given locker.

### Parameters

#### lockerid

The **lockerid** parameter represents a locker returned by `envM;lock_id()`.

#### priority

Upon return, the **priority** parameter will point to a value between 0 and 2^32-1.

### Errors

The `DB_ENV->get_lk_priority()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="lock.md" class="link" title="Chapter 6.  The DB_LOCK Handle">DB_LOCK</a>

### See Also

<a href="lock.md#locklist" class="xref" title="Locking Subsystem and Related Methods">Locking Subsystem and Related Methods</a>, <a href="envset_lk_priority.md" class="xref" title="DB_ENV-&gt;set_lk_priority()">DB_ENV-&gt;set_lk_priority()</a>
