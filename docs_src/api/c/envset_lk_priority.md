---
title: "DB_ENV->set_lk_priority()"
api-name: "DB_ENV->set_lk_priority()"
source: docs/api_reference/C/envset_lk_priority.html
---
## DB_ENV-\>set_lk_priority()

``` c
#include <db.h>

int
DB_ENV->set_lk_priority(DB_ENV *dbenv,
    u_int32_t lockerid, u_int32_t priority);  
```

Set the priority of the given locker. This value is used when resolving deadlocks, the deadlock resolution algorithm will reject a lock request from a locker with a lower priority before a request from a locker with a higher priority.

By default, all lockers are created with a priority of 100.

The `DB_ENV->set_lk_priority()` method may be called at any time during the life of the application.

The `DB_ENV->set_lk_priority()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### lockerid

The **lockerid** parameter represents a locker returned by `DB_ENV->lock_id()`.

#### priority

The **priority** parameter must be a value between 0 and 2^32-1.

### Errors

The `DB_ENV->set_lk_priority()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="lock.md" class="link" title="Chapter 6.  The DB_LOCK Handle">DB_LOCK</a>

### See Also

<a href="lock.md#locklist" class="xref" title="Locking Subsystem and Related Methods">Locking Subsystem and Related Methods</a>
