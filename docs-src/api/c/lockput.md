---
title: "DB_ENV->lock_put()"
api-name: "DB_ENV->lock_put()"
source: docs/api_reference/C/lockput.html
---
## DB_ENV-\>lock_put()

``` c
#include <db.h>

int
DB_ENV->lock_put(DB_ENV *env, DB_LOCK *lock);  
```

The `DB_ENV->lock_put()` method releases **lock**.

The `DB_ENV->lock_put()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### lock

The **lock** parameter is the lock to be released.

### Errors

The `DB_ENV->lock_put()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="lock.md" class="link" title="Chapter 6.  The DB_LOCK Handle">DB_LOCK</a>

### See Also

<a href="lock.md#locklist" class="xref" title="Locking Subsystem and Related Methods">Locking Subsystem and Related Methods</a>
