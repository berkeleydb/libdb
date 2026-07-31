---
title: "DB->sync()"
api-name: "DB->sync()"
source: docs/api_reference/C/dbsync.html
---
## DB-\>sync()

``` c
#include <db.h>

int
DB->sync(DB *db, u_int32_t flags);  
```

The `DB->sync()` method flushes any cached information to disk. This method operates on the database file level, so if the file contains multiple database handles then this method will flush to disk any information that is cached for any of those handles.

If the database is in memory only, the `DB->sync()` method has no effect and will always succeed.

**It is important to understand that flushing cached information to disk only minimizes the window of opportunity for corrupted data.** Although unlikely, it is possible for database corruption to happen if a system or application crash occurs while writing data to the database. To ensure that database corruption never occurs, applications must either: use transactions and logging with automatic recovery; use logging and application-specific recovery; or edit a copy of the database, and once all applications using the database have successfully called <a href="dbclose.md" class="xref" title="DB-&gt;close()">DB-&gt;close()</a>, atomically replace the original database with the updated copy.

The `DB->sync()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flags

The **flags** parameter is currently unused, and must be set to 0.

### Errors

The `DB->sync()` method may fail and return one of the following non-zero errors:

#### DB_REP_HANDLE_DEAD

When a client synchronizes with the master, it is possible for committed transactions to be rolled back. This invalidates all the database and cursor handles opened in the replication environment. Once this occurs, an attempt to use such a handle will return `DB_REP_HANDLE_DEAD`. The application will need to discard the handle and open a new one in order to continue processing.

#### DB_REP_LOCKOUT

The operation was blocked by client/master synchronization.

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
