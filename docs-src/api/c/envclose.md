---
title: "DB_ENV->close()"
api-name: "DB_ENV->close()"
source: docs/api_reference/C/envclose.html
---
## DB_ENV-\>close()

``` c
#include <db.h>

int
DB_ENV->close(DB_ENV *dbenv, u_int32_t flags);  
```

The `DB_ENV->close()` method closes the Berkeley DB environment, freeing any allocated resources and closing all database handles opened with this environment handle, as well as closing any underlying subsystems.

When you call the `DB_ENV->close()` method, all open <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handles and <a href="dbc.md" class="link" title="Chapter 3.  The DBcursor Handle">DBcursor</a> handles are closed automatically by this function. And, when you close a database handle, all cursors opened with it are closed automatically.

In multiple threads of control, each thread of control opens a database environment and the database handles within it. When you close each database handle using the `DB_ENV->close()` method, by default, the database is not synchronized and is similar to calling the `DB->close(DB_NOSYNC)` method. This is to avoid unncessary database synchronization when there are multiple environment handles open. To ensure all open database handles are synchronized when you close the last environment handle, set the flag parameter value of the `DB_ENV->close()` method to DB_FORCESYNC. This is similar to calling the `DB->close(0)` method to close each database handle.

If a database close operation fails, the method returns a non-zero error value for the first instance of such an error, and continues to close the rest of the database and environment handles.

The <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle should not be closed while any other handle that refers to it is not yet closed; for example, database environment handles must not be closed while transactions in the environment have not yet been committed or aborted. Specifically, this includes the <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a>, <a href="logc.md" class="link" title="The DB_LOGC Handle">DB_LOGC</a> and <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a> handles.

Where the environment was initialized with the <a href="envopen.md#envopen_DB_INIT_LOCK" class="link">DB_INIT_LOCK</a> flag, calling `DB_ENV->close()` does not release any locks still held by the closing process, providing functionality for long-lived locks. Processes that want to have all their locks released can do so by issuing the appropriate <a href="lockvec.md" class="xref" title="DB_ENV-&gt;lock_vec()">DB_ENV-&gt;lock_vec()</a> call.

Where the environment was initialized with the <a href="envopen.md#envopen_DB_INIT_MPOOL" class="link">DB_INIT_MPOOL</a> flag, calling `DB_ENV->close()` implies calls to <a href="mempfclose.md" class="xref" title="DB_MPOOLFILE-&gt;close()">DB_MPOOLFILE-&gt;close()</a> for any remaining open files in the memory pool that were returned to this process by calls to <a href="mempfopen.md" class="xref" title="DB_MPOOLFILE-&gt;open()">DB_MPOOLFILE-&gt;open()</a>. It does not imply a call to <a href="mempfsync.md" class="xref" title="DB_MPOOLFILE-&gt;sync()">DB_MPOOLFILE-&gt;sync()</a> for those files.

Where the environment was initialized with the <a href="envopen.md#envopen_DB_INIT_TXN" class="link">DB_INIT_TXN</a> flag, calling `DB_ENV->close()` aborts any unresolved transactions. Applications should not depend on this behavior for transactions involving Berkeley DB databases; all such transactions should be explicitly resolved. The problem with depending on this semantic is that aborting an unresolved transaction involving database operations requires a database handle. Because the database handles should have been closed before calling `DB_ENV->close()`, it will not be possible to abort the transaction, and recovery will have to be run on the Berkeley DB environment before further operations are done.

Where log cursors were created using the <a href="logcursor.md" class="xref" title="DB_ENV-&gt;log_cursor()">DB_ENV-&gt;log_cursor()</a> method, calling `DB_ENV->close()` does not imply closing those cursors.

In multithreaded applications, only a single thread may call the `DB_ENV->close()` method.

After `DB_ENV->close()` has been called, regardless of its return, the Berkeley DB environment handle may not be accessed again.

The `DB_ENV->close()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flags

The **flags** parameter must be set to 0 or be set to the following value:

- `DB_FORCESYNC`

  When closing each database handle internally, synchronize the database. If this flag is not specified, the database handle is closed without synchronizing the database.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
