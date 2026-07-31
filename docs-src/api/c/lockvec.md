---
title: "DB_ENV->lock_vec()"
api-name: "DB_ENV->lock_vec()"
source: docs/api_reference/C/lockvec.html
---
## DB_ENV-\>lock_vec()

``` c
#include <db.h>

int
DB_ENV->lock_vec(DB_ENV *env, u_int32_t locker, u_int32_t flags,
    DB_LOCKREQ list[], int nlist, DB_LOCKREQ **elistp);  
```

The `DB_ENV->lock_vec()` method atomically obtains and releases one or more locks from the lock table. The `DB_ENV->lock_vec()` method is intended to support acquisition or trading of multiple locks under one lock table semaphore, as is needed for lock coupling or in multigranularity locking for lock escalation.

If any of the requested locks cannot be acquired, or any of the locks to be released cannot be released, the operations before the failing operation are guaranteed to have completed successfully, and `DB_ENV->lock_vec()` returns a non-zero value. In addition, if **elistp** is not NULL, it is set to point to the DB_LOCKREQ entry that was being processed when the error occurred.

Unless otherwise specified, the `DB_ENV->lock_vec()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### locker

The **locker** parameter is an unsigned 32-bit integer quantity. It represents the entity requesting or releasing the lock.

#### flags

The **flags** parameter must be set to 0 or the following value:

- `DB_LOCK_NOWAIT`

  If a lock cannot be granted because the requested lock conflicts with an existing lock, return DB_LOCK_NOTGRANTED immediately instead of waiting for the lock to become available. In this case, if non-NULL, **elistp** identifies the request that was not granted.

#### list

The **list** array provided to `DB_ENV->lock_vec()` is typedef'd as DB_LOCKREQ.

To ensure compatibility with future releases of Berkeley DB, all fields of the DB_LOCKREQ structure that are not explicitly set should be initialized to 0 before the first time the structure is used. Do this by declaring the structure external or static, or by calling **memset**(3).

A DB_LOCKREQ structure has at least the following fields:

- `lockop_t op;`

  The operation to be performed, which must be set to one of the following values:

  - `DB_LOCK_GET`

    Get the lock defined by the values of the **mode** and **obj** structure fields, for the specified **locker**. Upon return from `DB_ENV->lock_vec()`, if the **lock** field is non-NULL, a reference to the acquired lock is stored there. (This reference is invalidated by any call to `DB_ENV->lock_vec()` or <a href="lockput.md" class="xref" title="DB_ENV-&gt;lock_put()">DB_ENV-&gt;lock_put()</a> that releases the lock.)

  - `DB_LOCK_GET_TIMEOUT`

    Identical to DB_LOCK_GET except that the value in the **timeout** structure field overrides any previously specified timeout value for this lock. A value of 0 turns off any previously specified timeout.

  - `DB_LOCK_PUT`

    The lock to which the **lock** structure field refers is released. The **locker** parameter, and **mode** and **obj** fields are ignored.

  - `DB_LOCK_PUT_ALL`

    All locks held by the specified **locker** are released. The **lock**, **mode**, and **obj** structure fields are ignored. Locks acquired in operations performed by the current call to `DB_ENV->lock_vec()` which appear before the DB_LOCK_PUT_ALL operation are released; those acquired in operations appearing after the DB_LOCK_PUT_ALL operation are not released.

  - `DB_LOCK_PUT_OBJ`

    All locks held on **obj** are released. The **locker** parameter and the **lock** and **mode** structure fields are ignored. Locks acquired in operations performed by the current call to `DB_ENV->lock_vec()` that appear before the DB_LOCK_PUT_OBJ operation are released; those acquired in operations appearing after the DB_LOCK_PUT_OBJ operation are not released.

  - `DB_LOCK_TIMEOUT`

    Cause the specified **locker** to timeout immediately. If the database environment has not configured automatic deadlock detection, the transaction will timeout the next time deadlock detection is performed. As transactions acquire locks on behalf of a single locker ID, timing out the locker ID associated with a transaction will time out the transaction itself.

- `DB_LOCK lock;`

  A lock reference.

- `const lockmode_t mode;`

  The lock mode, used as an index into the environment's lock conflict matrix. When using the default lock conflict matrix, **mode** must be set to one of the following values:

  - `DB_LOCK_READ`

    read (shared)

  - `DB_LOCK_WRITE`

    write (exclusive)

  - `DB_LOCK_IWRITE`

    intention to write (shared)

  - `DB_LOCK_IREAD`

    intention to read (shared)

  - `DB_LOCK_IWR`

    intention to read and write (shared)

  See <a href="envset_lk_conflicts.md" class="xref" title="DB_ENV-&gt;set_lk_conflicts()">DB_ENV-&gt;set_lk_conflicts()</a> and <a href="../../programmer_reference/lock_stdmode.html" class="olink">Standard Lock Modes</a> for more information on the lock conflict matrix.

- ****const DBT obj;****

  An untyped byte string that specifies the object to be locked or released. Applications using the locking subsystem directly while also doing locking via the Berkeley DB access methods must take care not to inadvertently lock objects that happen to be equal to the unique file IDs used to lock files. See <a href="../../programmer_reference/lock_am_conv.html" class="olink">Access method locking conventions</a> in the *Berkeley DB Programmer's Reference Guide* for more information.

- **u_int32_t timeout;**

  The lock timeout value.

#### nlist

The **nlist** parameter specifies the number of elements in the **list** array.

#### elistp

If an error occurs, and the **elistp** parameter is non-NULL, it is set to point to the DB_LOCKREQ entry that was being processed when the error occurred.

### Errors

The `DB_ENV->lock_vec()` method may fail and return one of the following non-zero errors:

#### DB_LOCK_DEADLOCK

A transactional database environment operation was selected to resolve a deadlock.

#### DB_LOCK_NOTGRANTED

A Berkeley DB Concurrent Data Store database environment configured for lock timeouts was unable to grant a lock in the allowed time.

You attempted to open a database handle that is configured for no waiting exclusive locking, but the exclusive lock could not be immediately obtained. See <a href="dbset_lk_exclusive.md" class="xref" title="DB-&gt;set_lk_exclusive()">DB-&gt;set_lk_exclusive()</a> for more information.

#### DB_LOCK_NOTGRANTED

The <a href="lockvec.md#vec_DB_LOCK_NOWAIT" class="link">DB_LOCK_NOWAIT</a> flag or lock timers were configured and the lock could not be granted before the wait-time expired.

#### EINVAL

An invalid flag value or parameter was specified.

#### ENOMEM

The maximum number of locks has been reached.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="lock.md" class="link" title="Chapter 6.  The DB_LOCK Handle">DB_LOCK</a>

### See Also

<a href="lock.md#locklist" class="xref" title="Locking Subsystem and Related Methods">Locking Subsystem and Related Methods</a>
