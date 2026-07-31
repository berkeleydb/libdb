---
title: "DB_ENV->set_lk_detect()"
api-name: "DB_ENV->set_lk_detect()"
source: docs/api_reference/C/envset_lk_detect.html
---
## DB_ENV-\>set_lk_detect()

``` c
#include <db.h>

int
DB_ENV->set_lk_detect(DB_ENV *dbenv, u_int32_t detect);  
```

Set if the deadlock detector is to be run whenever a lock conflict occurs, and specify what lock request(s) should be rejected. As transactions acquire locks on behalf of a single locker ID, rejecting a lock request associated with a transaction normally requires the transaction be aborted.

The database environment's deadlock detector configuration may also be configured using the environment's <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "set_lk_detect", one or more whitespace characters, and the method **detect** parameter as a string; for example, "set_lk_detect DB_LOCK_OLDEST". Because the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

The `DB_ENV->set_lk_detect()` method configures a database environment, not only operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle.

The `DB_ENV->set_lk_detect()` method may be called either before or after environment open, but once it is set it may not be changed again during the environment's lifetime.

The `DB_ENV->set_lk_detect()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### detect

The **detect** parameter configures the deadlock detector. The deadlock detector will reject the lock request with the lowest priority. If multiple lock requests have the lowest priority, then the **detect** parameter is used to select which of those lock requests to reject. The specified value must be one of the following list:

- `DB_LOCK_DEFAULT`

  Use whatever lock policy was specified when the database environment was created. If no lock policy has yet been specified, set the lock policy to DB_LOCK_RANDOM.

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

### Errors

The `DB_ENV->set_lk_detect()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="lock.md" class="link" title="Chapter 6.  The DB_LOCK Handle">DB_LOCK</a>

### See Also

<a href="lock.md#locklist" class="xref" title="Locking Subsystem and Related Methods">Locking Subsystem and Related Methods</a>
