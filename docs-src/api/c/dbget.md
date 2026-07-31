---
title: "DB->get()"
api-name: "DB->get()"
source: docs/api_reference/C/dbget.html
---
## DB-\>get()

``` c
#include <db.h>

int
DB->get(DB *db,
    DB_TXN *txnid, DBT *key, DBT *data, u_int32_t flags);

int
DB->pget(DB *db,
    DB_TXN *txnid, DBT *key, DBT *pkey, DBT *data, u_int32_t flags);  
```

The `DB->get()` method retrieves key/data pairs from the database. The address and length of the data associated with the specified **key** are returned in the structure to which **data** refers.

In the presence of duplicate key values, `DB->get()` will return the first data item for the designated key. Duplicates are sorted by:

- Their sort order, if a duplicate sort function was specified.

- Any explicit cursor designated insertion.

- By insert order. This is the default behavior.

**Retrieval of duplicates requires the use of cursor operations.** See <a href="dbcget.md" class="xref" title="DBcursor-&gt;get()">DBcursor-&gt;get()</a> for details.

When called on a database that has been made into a secondary index using the <a href="dbassociate.md" class="xref" title="DB-&gt;associate()">DB-&gt;associate()</a> method, the `DB->get()` and `DB->pget()` methods return the key from the secondary index and the data item from the primary database. In addition, the `DB->pget()` method returns the key from the primary database. In databases that are not secondary indices, the `DB->pget()` method will always fail.

The `DB->get()` method will return <a href="../../guides/programmer_reference/program_errorret.md#program_errorret.DB_NOTFOUND" class="olink">DB_NOTFOUND</a> if the specified key is not in the database. The `DB->get()` method will return <a href="../../guides/programmer_reference/program_errorret.md#program_errorret.DB_KEYEMPTY" class="olink">DB_KEYEMPTY</a> if the database is a Queue or Recno database and the specified key exists, but was never explicitly created by the application or was later deleted. Unless otherwise specified, the `DB->get()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### txnid

If the operation is part of an application-specified transaction, the **txnid** parameter is a transaction handle returned from <a href="txnbegin.md" class="xref" title="DB_ENV-&gt;txn_begin()">DB_ENV-&gt;txn_begin()</a>; if the operation is part of a Berkeley DB Concurrent Data Store group, the **txnid** parameter is a handle returned from <a href="envcdsgroup_begin.md" class="xref" title="DB_ENV-&gt;cdsgroup_begin()">DB_ENV-&gt;cdsgroup_begin()</a>; otherwise NULL. If no transaction handle is specified, but the operation occurs in a transactional database, the operation will be implicitly transaction protected.

#### key

The key <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> operated on.

If <a href="dbt.md#dbt_DB_DBT_PARTIAL" class="link">DB_DBT_PARTIAL</a> is set for the DBT used for this parameter, and if the **flags** parameter is not set to <a href="dbget.md#dbget_DB_CONSUME" class="link">DB_CONSUME</a> <a href="dbget.md#dbget_DB_CONSUME_WAIT" class="link">DB_CONSUME_WAIT</a>, or <a href="dbget.md#dbget_DB_SET_RECNO" class="link">DB_SET_RECNO</a>, then this method will fail and return `EINVAL`.

#### pkey

The **pkey** parameter is the return key from the primary database. If <a href="dbt.md#dbt_DB_DBT_PARTIAL" class="link">DB_DBT_PARTIAL</a> is set for the DBT used for this parameter, then this method will fail and return `EINVAL`.

#### data

The data <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> operated on.

#### flags

The **flags** parameter must be set to 0 or one of the following values:

- `DB_CONSUME`

  Return the record number and data from the available record closest to the head of the queue, and delete the record. The record number will be returned in **key**, as described in <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a>. The data will be returned in the **data** parameter. A record is available if it is not deleted and is not currently locked. The underlying database must be of type Queue for DB_CONSUME to be specified.

- `DB_CONSUME_WAIT`

  The DB_CONSUME_WAIT flag is the same as the DB_CONSUME flag, except that if the Queue database is empty, the thread of control will wait until there is data in the queue before returning. The underlying database must be of type Queue for DB_CONSUME_WAIT to be specified.

  If lock or transaction timeouts have been specified, the `DB->get()` method with the DB_CONSUME_WAIT flag may return <a href="../../guides/programmer_reference/program_errorret.md#program_errorret.DB_LOCK_NOTGRANTED" class="olink">DB_LOCK_NOTGRANTED</a>. This failure, by itself, does not require the enclosing transaction be aborted.

- `DB_GET_BOTH`

  Retrieve the key/data pair only if both the key and data match the arguments.

  When using a secondary index handle, the `DB_GET_BOTH`: flag causes:

  - the `DB->pget()` version of this method to retun the secondary key/primary key/data tuple only if both the primary and secondary keys match the arguments.

  - the `DB->get()` version of this method to result in an error.

- `DB_SET_RECNO`

  Retrieve the specified numbered key/data pair from a database. Upon return, both the **key** and **data** items will have been filled in.

  The **data** field of the specified **key** must be a pointer to a logical record number (that is, a **db_recno_t**). This record number determines the record to be retrieved.

  For DB_SET_RECNO to be specified, the underlying database must be of type Btree, and it must have been created with the DB_RECNUM flag.

In addition, the following flags may be set by bitwise inclusively **OR**'ing them into the **flags** parameter:

- `DB_IGNORE_LEASE`

  Return the data item irrespective of the state of master leases. The item will be returned under all conditions: if master leases are not configured, if the request is made to a client, if the request is made to a master with a valid lease, or if the request is made to a master without a valid lease.

- `DB_MULTIPLE`

  Return multiple data items in the buffer to which the **data** parameter refers.

  In the case of Btree or Hash databases, all of the data items associated with the specified key are entered into the buffer. In the case of Queue, Recno or Heap databases, all of the data items in the database, starting at, and subsequent to, the specified key, are entered into the buffer.

  The buffer to which the **data** parameter refers must be provided from user memory (see <a href="dbt.md#dbt_DB_DBT_USERMEM" class="link">DB_DBT_USERMEM</a>). The buffer must be at least as large as the page size of the underlying database, aligned for unsigned integer access, and be a multiple of 1024 bytes in size. If the buffer size is insufficient, then upon return from the call the size field of the **data** parameter will have been set to an estimated buffer size, and the error DB_BUFFER_SMALL is returned. (The size is an estimate as the exact size needed may not be known until all entries are read. It is best to initially provide a relatively large buffer, but applications should be prepared to resize the buffer as necessary and repeatedly call the method.)

  The DB_MULTIPLE flag may only be used alone, or with the DB_GET_BOTH and DB_SET_RECNO options. The DB_MULTIPLE flag may not be used when accessing databases made into secondary indices using the <a href="dbassociate.md" class="xref" title="DB-&gt;associate()">DB-&gt;associate()</a> method.

  See the <a href="dbt.md#dbtlist" class="xref" title="DBT and Bulk Operations">DBT and Bulk Operations</a> for more information on working with bulk get.

- `DB_READ_COMMITTED`

  Configure a transactional get operation to have degree 2 isolation (the read is not repeatable).

- `DB_READ_UNCOMMITTED`

  Configure a transactional get operation to have degree 1 isolation, reading modified but not yet committed data. Silently ignored if the <a href="dbopen.md#dbopen_DB_READ_UNCOMMITTED" class="link">DB_READ_UNCOMMITTED</a> flag was not specified when the underlying database was opened.

- `DB_RMW`

  Acquire write locks instead of read locks when doing the read, if locking is configured. Setting this flag can eliminate deadlock during a read-modify-write cycle by acquiring the write lock during the read part of the cycle so that another thread of control acquiring a read lock for the same item, in its own read-modify-write cycle, will not result in deadlock.

  Because the `DB->get()` method will not hold locks across Berkeley DB calls in non-transactional operations, the <a href="dbcget.md#dbcget_DB_RMW" class="link">DB_RMW</a> flag to the `DB->get()` call is meaningful only in the presence of transactions.

### Errors

The `DB->get()` method may fail and return one of the following non-zero errors:

#### DB_BUFFER_SMALL

The requested item could not be returned due to undersized buffer.

#### DB_LOCK_DEADLOCK

A transactional database environment operation was selected to resolve a deadlock.

#### DB_LOCK_NOTGRANTED

A Berkeley DB Concurrent Data Store database environment configured for lock timeouts was unable to grant a lock in the allowed time.

You attempted to open a database handle that is configured for no waiting exclusive locking, but the exclusive lock could not be immediately obtained. See <a href="dbset_lk_exclusive.md" class="xref" title="DB-&gt;set_lk_exclusive()">DB-&gt;set_lk_exclusive()</a> for more information.

#### DB_LOCK_NOTGRANTED

The `DB_CONSUME_WAIT` flag was specified, lock or transaction timers were configured and the lock could not be granted before the wait-time expired.

#### DB_REP_HANDLE_DEAD

When a client synchronizes with the master, it is possible for committed transactions to be rolled back. This invalidates all the database and cursor handles opened in the replication environment. Once this occurs, an attempt to use such a handle will return `DB_REP_HANDLE_DEAD`. The application will need to discard the handle and open a new one in order to continue processing.

#### DB_REP_LEASE_EXPIRED

The operation failed because the site's replication master lease has expired.

#### DB_REP_LOCKOUT

The operation was blocked by client/master synchronization.

#### DB_SECONDARY_BAD

A secondary index references a nonexistent primary key.

#### EINVAL

If a record number of 0 was specified; the <a href="envopen.md#envopen_DB_THREAD" class="link">DB_THREAD</a> flag was specified to the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method and none of the <a href="dbt.md#dbt_DB_DBT_MALLOC" class="link">DB_DBT_MALLOC</a>, <a href="dbt.md#dbt_DB_DBT_REALLOC" class="link">DB_DBT_REALLOC</a> or <a href="dbt.md#dbt_DB_DBT_USERMEM" class="link">DB_DBT_USERMEM</a> flags were set in the <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a>; the `DB->pget()` method was called with a <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle that does not refer to a secondary index; or if an invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
