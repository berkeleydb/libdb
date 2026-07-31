---
title: "DB->close()"
api-name: "DB->close()"
source: docs/api_reference/C/dbclose.html
---
## DB-\>close()

``` c
#include <db.h>

int
DB->close(DB *db, u_int32_t flags);  
```

The `DB->close()` method flushes cached database information to disk, closes any open cursors, frees allocated resources, and closes underlying files. When the close operation for a cursor fails, the method returns a non-zero error value for the first instance of such an error, and continues to close the rest of the cursors and database handles.

Although closing a database handle will close any open cursors, it is recommended that applications explicitly close all their <a href="dbc.md" class="link" title="Chapter 3.  The DBcursor Handle">DBcursor</a> handles before closing the database. The reason why is that when the cursor is explicitly closed, the memory allocated for it is reclaimed; however, this will <span class="emphasis">*not*</span> happen if you close a database while cursors are still opened.

The same rule, for the same reasons, hold true for <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a> handles. Simply make sure you close all your transaction handles before closing your database handle.

Because key/data pairs are cached in memory, applications should make a point to always either close database handles or sync their data to disk (using the <a href="dbsync.md" class="xref" title="DB-&gt;sync()">DB-&gt;sync()</a> method) before exiting, to ensure that any data cached in main memory are reflected in the underlying file system.

When called on a database that is the primary database for a secondary index, the primary database should be closed only after all secondary indices referencing it have been closed.

When multiple threads are using the <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> concurrently, only a single thread may call the `DB->close()` method.

The <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle may not be accessed again after `DB->close()` is called, regardless of its return.

If you do not close the <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle explicitly, it will be closed when the environment handle that owns the <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle is closed.

The `DB->close()` method returns a non-zero error value on failure and 0 on success. The error values that `DB->close()` method returns include the error values of `DBcursor->close()` and the following:

#### DB_LOCK_DEADLOCK

A transactional database environment operation was selected to resolve a deadlock.

#### DB_LOCK_NOTGRANTED

A Berkeley DB Concurrent Data Store database environment configured for lock timeouts was unable to grant a lock in the allowed time.

You attempted to open a database handle that is configured for no waiting exclusive locking, but the exclusive lock could not be immediately obtained. See <a href="dbset_lk_exclusive.md" class="xref" title="DB-&gt;set_lk_exclusive()">DB-&gt;set_lk_exclusive()</a> for more information.

#### EINVAL

If the cursor is already closed; or if an invalid flag value or parameter was specified.

### Parameters

#### flags

The **flags** parameter must be set to 0 or be set to the following value:

- `DB_NOSYNC`

  Do not flush cached information to disk. This flag is a dangerous option. It should be set only if the application is doing logging (with transactions) so that the database is recoverable after a system or application crash, or if the database is always generated from scratch after any system or application crash.

  **It is important to understand that flushing cached information to disk only minimizes the window of opportunity for corrupted data.** Although unlikely, it is possible for database corruption to happen if a system or application crash occurs while writing data to the database. To ensure that database corruption never occurs, applications must either: use transactions and logging with automatic recovery; use logging and application-specific recovery; or edit a copy of the database, and once all applications using the database have successfully called `DB->close()`, atomically replace the original database with the updated copy.

  Note that this flag only works when the database has been opened using an environment.

### Errors

The `DB->close()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

The error messages returned for the first error encountered when `DB->close()` method closes any open cursors include:

#### DB_LOCK_DEADLOCK

A transactional database environment operation was selected to resolve a deadlock.

#### DB_LOCK_NOTGRANTED

A Berkeley DB Concurrent Data Store database environment configured for lock timeouts was unable to grant a lock in the allowed time.

You attempted to open a database handle that is configured for no waiting exclusive locking, but the exclusive lock could not be immediately obtained. See <a href="dbset_lk_exclusive.md" class="xref" title="DB-&gt;set_lk_exclusive()">DB-&gt;set_lk_exclusive()</a> for more information.

#### EINVAL

If the cursor is already closed; or if an invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
