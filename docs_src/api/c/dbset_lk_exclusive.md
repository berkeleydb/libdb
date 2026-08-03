---
title: "DB->set_lk_exclusive()"
api-name: "DB->set_lk_exclusive()"
source: docs/api_reference/C/dbset_lk_exclusive.html
---
## DB-\>set_lk_exclusive()

``` c
#include <db.h>

int
DB->set_lk_exclusive(DB *db, int nowait_onoff);  
```

Configures the database handle to obtain a write lock on the entire database when it is opened. This gives the handle exclusive access to the database, because the write lock will block all other threads of control for both read and write access.

Use this method to improve the throughput performance on your database for the thread that is controlling this handle. When configured with this method, operations on the database do not acquire page locks as they perform read and/or write operations. Also, the exclusive lock means that operations performed on the database handle will never be blocked waiting for lock due to another thread's activities. The application will also be immune to deadlocks.

On the other hand, use of this method means that you can only have a single thread accessing the database until the handle is closed. For some applications, the loss of multiple threads concurrently operating on the database will result in performance degradation.

Also, use of this method means that you can only have one transaction active for the handle at a time.

### Note

This method is incompatible with the <a href="dbopen.md#open_DB_THREAD" class="link">DB_THREAD</a> configuration flag.

The `DB->set_lk_exclusive()` method may not be called after the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called.

The `DB->set_lk_exclusive()` method returns a non-zero error value on failure and 0 on success.

### Replication Notes

Replication applications that use exclusive database handles need to be written with caution. This is because replication clients cannot process updates on an exclusive database until all local handles on the database are closed. Also, attempting to open an exclusive database handle on a currently operating client will result in the open call failing with the error `EINVAL`.

Also, opening an exclusive database handle on a replication master will result in all clients being locked out of the database. On clients, existing handles on the exclusive database will return the error `DB_REP_DEAD_HANDLE` when accessed, and must be closed. New handles opened on the exclusive database will block until the master closes its exclusive database handle.

### Parameters

#### nowait_onoff

If set to `0`, this method will block until it can obtain the exclusive lock on the database. If set to some value other than `0`, `DB_LOCK_NOTGRANTED` is returned when the handle is opened if the exclusive database lock cannot be immediately obtained.

### Errors

The `DB->set_lk_exclusive()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called; the method was called on a currently operating replication client; or if an invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
