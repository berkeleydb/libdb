---
title: "DBcursor->close()"
api-name: "DBcursor->close()"
source: docs/api_reference/C/dbcclose.html
---
## DBcursor-\>close()

``` c
#include <db.h>

int
DBcursor->close(DBC *DBcursor);  
```

The `DBcursor->close()` method discards the cursor.

It is possible for the `DBcursor->close()` method to return <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_LOCK_DEADLOCK" class="olink">DB_LOCK_DEADLOCK</a>, signaling that any enclosing transaction should be aborted. If the application is already intending to abort the transaction, this error should be ignored, and the application should proceed.

After the `DBcursor->close()` method has been called, regardless of its return value, you can not use the cursor handle again.

It is not required to close the cursor explicitly before closing the database handle or the transaction handle that owns this cursor because, closing a database handle or a transaction handle closes those open cursors.

However, it is recommended that you always close all cursor handles immediately after their use to promote concurrency and to release resources such as page locks.

The `DBcursor->close()` method returns a non-zero error value on failure and 0 on success.

### Errors

The `DBcursor->close()` method may fail and return one of the following non-zero errors:

#### DB_LOCK_DEADLOCK

A transactional database environment operation was selected to resolve a deadlock.

#### DB_LOCK_NOTGRANTED

A Berkeley DB Concurrent Data Store database environment configured for lock timeouts was unable to grant a lock in the allowed time.

You attempted to open a database handle that is configured for no waiting exclusive locking, but the exclusive lock could not be immediately obtained. See <a href="dbset_lk_exclusive.md" class="xref" title="DB-&gt;set_lk_exclusive()">DB-&gt;set_lk_exclusive()</a> for more information.

#### EINVAL

If the cursor is already closed; or if an invalid flag value or parameter was specified.

### Class

<a href="dbc.md" class="link" title="Chapter 3.  The DBcursor Handle">DBcursor</a>

### See Also

<a href="dbc.md#dbclist" class="xref" title="Database Cursors and Related Methods">Database Cursors and Related Methods</a>
