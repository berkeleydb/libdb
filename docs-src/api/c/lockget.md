---
title: "DB_ENV->lock_get()"
api-name: "DB_ENV->lock_get()"
source: docs/api_reference/C/lockget.html
---
## DB_ENV-\>lock_get()

``` c
#include <db.h>

int
DB_ENV->lock_get(DB_ENV *env, u_int32_t locker,
    u_int32_t flags, const DBT *object,
    const db_lockmode_t lock_mode, DB_LOCK *lock);  
```

The `DB_ENV->lock_get()` method acquires a lock from the lock table, returning information about it in the **lock** parameter.

The `DB_ENV->lock_get()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### locker

The **locker** parameter is an unsigned 32-bit integer quantity. It represents the entity requesting the lock.

#### flags

The **flags** parameter must be set to 0 or the following value:

- `DB_LOCK_NOWAIT`

  If a lock cannot be granted because the requested lock conflicts with an existing lock, return DB_LOCK_NOTGRANTED immediately instead of waiting for the lock to become available.

#### object

The **object** parameter is an untyped byte string that specifies the object to be locked. Applications using the locking subsystem directly while also doing locking via the Berkeley DB access methods must take care not to inadvertently lock objects that happen to be equal to the unique file IDs used to lock files. See <a href="../../programmer_reference/lock_am_conv.html" class="olink">Access method locking conventions</a> in the *Berkeley DB Programmer's Reference Guide* for more information.

#### lock_mode

The **lock_mode** parameter is used as an index into the environment's lock conflict matrix. When using the default lock conflict matrix, **lock_mode** must be set to one of the following values:

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

#### lock

The `DB_ENV->lock_get()` method returns the lock information in **lock**.

### Errors

The `DB_ENV->lock_get()` method may fail and return one of the following non-zero errors:

#### DB_LOCK_DEADLOCK

A transactional database environment operation was selected to resolve a deadlock.

#### DB_LOCK_NOTGRANTED

A Berkeley DB Concurrent Data Store database environment configured for lock timeouts was unable to grant a lock in the allowed time.

You attempted to open a database handle that is configured for no waiting exclusive locking, but the exclusive lock could not be immediately obtained. See <a href="dbset_lk_exclusive.md" class="xref" title="DB-&gt;set_lk_exclusive()">DB-&gt;set_lk_exclusive()</a> for more information.

#### DB_LOCK_NOTGRANTED

The <a href="lockvec.md#vec_DB_LOCK_NOWAIT" class="link">DB_LOCK_NOWAIT</a> flag or lock timers were configured and the lock could not be granted before the wait-time expired.

#### EINVAL

An invalid flag value or parameter was specified.

#### EINVAL

The method was called on an environment which had been opened without being configured for locking.

#### ENOMEM

The maximum number of locks has been reached.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="lock.md" class="link" title="Chapter 6.  The DB_LOCK Handle">DB_LOCK</a>

### See Also

<a href="lock.md#locklist" class="xref" title="Locking Subsystem and Related Methods">Locking Subsystem and Related Methods</a>
