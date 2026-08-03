---
title: "DB_ENV->lock_id()"
api-name: "DB_ENV->lock_id()"
source: docs/api_reference/C/lockid.html
---
## DB_ENV-\>lock_id()

``` c
#include <db.h>

int
DB_ENV->lock_id(DB_ENV *env, u_int32_t *idp);  
```

The `DB_ENV->lock_id()` method copies a locker ID, which is guaranteed to be unique in the environment's lock table, into the memory location to which **idp** refers.

Note that lockers are not free-threaded; lockers can not be used by more than one thread at the same time.

The <a href="lockid_free.md" class="xref" title="DB_ENV-&gt;lock_id_free()">DB_ENV-&gt;lock_id_free()</a> method should be called to return the locker ID to the Berkeley DB library when it is no longer needed.

The `DB_ENV->lock_id()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### idp

The **idp** parameter references memory into which the allocated locker ID is copied.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="lock.md" class="link" title="Chapter 6.  The DB_LOCK Handle">DB_LOCK</a>

### See Also

<a href="lock.md#locklist" class="xref" title="Locking Subsystem and Related Methods">Locking Subsystem and Related Methods</a>
