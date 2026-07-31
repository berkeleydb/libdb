---
title: "DB_ENV->lock_detect()"
api-name: "DB_ENV->lock_detect()"
source: docs/api_reference/C/lockdetect.html
---
## DB_ENV-\>lock_detect()

``` c
#include <db.h>

int
DB_ENV->lock_detect(DB_ENV *env,
    u_int32_t flags, u_int32_t atype, int *rejected);  
```

The `DB_ENV->lock_detect()` method runs one iteration of the deadlock detector. The deadlock detector traverses the lock table and marks one of the participating lock requesters for rejection in each deadlock it finds.

The `DB_ENV->lock_detect()` method is the underlying method used by the <a href="db_deadlock.md" class="link" title="db_deadlock">db_deadlock</a> utility. See the <a href="db_deadlock.md" class="link" title="db_deadlock">db_deadlock</a> utility source code for an example of using `DB_ENV->lock_detect()` in a IEEE/ANSI Std 1003.1 (POSIX) environment.

The `DB_ENV->lock_detect()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flags

The **flags** parameter is currently unused, and must be set to 0.

#### atype

The **atype** parameter specifies which lock request(s) to reject. The deadlock detector will reject the lock request with the lowest priority. If multiple lock requests have the lowest priority, then the **atype** parameter is used to select which of those lock requests to reject. It must be set to one of the following list:

- `DB_LOCK_DEFAULT`

  Use the default lock policy, which is DB_LOCK_RANDOM.

- `DB_LOCK_EXPIRE`

  Reject lock requests which have timed out. No other deadlock detection is performed.

- `DB_LOCK_MAXLOCKS`

  Reject the lock request for the locker ID with the most locks.

- `DB_LOCK_MAXWRITE`

  Reject the lock request for the locker ID with the most write locks.

- `DB_LOCK_MINLOCKS`

  Reject the lock request for the locker ID with the fewest locks.

- `DB_LOCK_MINWRITE`

  Reject the lock request for the locker ID with the fewest write locks.

- `DB_LOCK_OLDEST`

  Reject the lock request for the locker ID with the oldest lock.

- `DB_LOCK_RANDOM`

  Reject the lock request for a random locker ID.

- `DB_LOCK_YOUNGEST`

  Reject the lock request for the locker ID with the youngest lock.

#### rejected

If the **rejected** parameter is non-NULL, the memory location to which it refers will be set to the number of lock requests that were rejected.

### Errors

The `DB_ENV->lock_detect()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="lock.md" class="link" title="Chapter 6.  The DB_LOCK Handle">DB_LOCK</a>

### See Also

<a href="lock.md#locklist" class="xref" title="Locking Subsystem and Related Methods">Locking Subsystem and Related Methods</a>
