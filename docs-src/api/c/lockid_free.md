---
title: "DB_ENV->lock_id_free()"
api-name: "DB_ENV->lock_id_free()"
source: docs/api_reference/C/lockid_free.html
---
## DB_ENV-\>lock_id_free()

``` c
#include <db.h>

int
DB_ENV->lock_id_free(DB_ENV *env, u_int32_t id);  
```

The `DB_ENV->lock_id_free()` method frees a locker ID allocated by the <a href="lockid.md" class="xref" title="DB_ENV-&gt;lock_id()">DB_ENV-&gt;lock_id()</a> method.

The `DB_ENV->lock_id_free()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### id

The **id** parameter is the locker id to be freed.

### Errors

The `DB_ENV->lock_id_free()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the locker ID is invalid or locks are still held by this locker ID; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="lock.md" class="link" title="Chapter 6.  The DB_LOCK Handle">DB_LOCK</a>

### See Also

<a href="lock.md#locklist" class="xref" title="Locking Subsystem and Related Methods">Locking Subsystem and Related Methods</a>
