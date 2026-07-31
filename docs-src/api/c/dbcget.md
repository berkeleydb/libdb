---
title: "DBcursor->get()"
api-name: "DBcursor->get()"
source: docs/api_reference/C/dbcget.html
---
## DBcursor-\>get()

``` c
#include <db.h>

int
DBcursor->get(DBC *DBcursor,
    DBT *key, DBT *data, u_int32_t flags);

int
DBcursor->pget(DBC *DBcursor,
    DBT *key, DBT *pkey, DBT *data, u_int32_t flags);  
```

The `DBcursor->get()` method retrieves key/data pairs from the database. The address and length of the key are returned in the object to which **key** refers (except for the case of the DB_SET flag, in which the **key** object is unchanged), and the address and length of the data are returned in the object to which **data** refers.

When called on a cursor opened on a database that has been made into a secondary index using the <a href="dbassociate.md" class="xref" title="DB-&gt;associate()">DB-&gt;associate()</a> method, the `DBcursor->get()` and `DBcursor->pget()` methods return the key from the secondary index and the data item from the primary database. In addition, the `DBcursor->pget()` method returns the key from the primary database. In databases that are not secondary indices, the `DBcursor->pget()` method will always fail.

Modifications to the database during a sequential scan will be reflected in the scan; that is, records inserted behind a cursor will not be returned while records inserted in front of a cursor will be returned.

In Queue and Recno databases, missing entries (that is, entries that were never explicitly created or that were created and then deleted) will be skipped during a sequential scan.

Unless otherwise specified, the `DBcursor->get()` method returns a non-zero error value on failure and 0 on success.

If `DBcursor->get()` fails for any reason, the state of the cursor will be unchanged.

### Parameters

#### key

The key <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> operated on.

If <a href="dbt.md#dbt_DB_DBT_PARTIAL" class="link">DB_DBT_PARTIAL</a> is set for the DBT used for this parameter, and if the **flags** parameter is set to <a href="dbcget.md#dbcget_DB_GET_BOTH" class="link">DB_GET_BOTH</a>, <a href="dbcget.md#dbcget_DB_GET_BOTH_RANGE" class="link">DB_GET_BOTH_RANGE</a>, <a href="dbcget.md#dbcget_DB_SET" class="link">DB_SET</a>, or <a href="dbcget.md#dbcget_DB_SET_RECNO" class="link">DB_SET_RECNO</a>, then this method will fail and return `EINVAL`.

#### pkey

The return key from the primary database. If <a href="dbt.md#dbt_DB_DBT_PARTIAL" class="link">DB_DBT_PARTIAL</a> is set for the DBT used for this parameter, then this method will fail and return `EINVAL`.

#### data

The data <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> operated on.

#### flags

The **flags** parameter must be set to one of the following values:

- `DB_CURRENT`

  Return the key/data pair to which the cursor refers.

  The `DBcursor->get()` method will return <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_KEYEMPTY" class="olink">DB_KEYEMPTY</a> if DB_CURRENT is set and the cursor key/data pair was deleted.

- `DB_FIRST`

  The cursor is set to refer to the first key/data pair of the database, and that pair is returned. If the first key has duplicate values, the first data item in the set of duplicates is returned.

  If the database is a Queue or Recno database, `DBcursor->get()` using the DB_FIRST flag will ignore any keys that exist but were never explicitly created by the application, or were created and later deleted.

  The `DBcursor->get()` method will return <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_NOTFOUND" class="olink">DB_NOTFOUND</a> if DB_FIRST is set and the database is empty.

- `DB_GET_BOTH`

  Move the cursor to the specified key/data pair of the database. The cursor is positioned to a key/data pair if both the key and data match the values provided on the key and data parameters.

  In all other ways, this flag is identical to the <a href="dbcget.md#dbcget_DB_SET" class="link">DB_SET</a> flag.

  When used with `DBcursor->pget()` on a secondary index handle, both the secondary and primary keys must be matched by the secondary and primary key item in the database. It is an error to use the DB_GET_BOTH flag with the `DBcursor->get()` version of this method and a cursor that has been opened on a secondary index handle.

- `DB_GET_BOTH_RANGE`

  Move the cursor to the specified key/data pair of the database. The key parameter must be an exact match with a key in the database. The data item retrieved is the item in a duplicate set that is the smallest value which is greater than or equal to the value provided by the data parameter (as determined by the comparison function). If this flag is specified on a database configured without sorted duplicate support, the behavior is identical to the `DB_GET_BOTH` flag. Returns the datum associated with the given key/data pair.

  In all other ways, this flag is identical to the <a href="dbcget.md#dbcget_DB_GET_BOTH" class="link">DB_GET_BOTH</a> flag.

- `DB_GET_RECNO`

  Return the record number associated with the cursor. The record number will be returned in **data**, as described in <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a>. The **key** parameter is ignored.

  For DB_GET_RECNO to be specified, the underlying database must be of type Btree, and it must have been created with the <a href="dbset_flags.md#dbset_flags_DB_RECNUM" class="link">DB_RECNUM</a> flag.

  When called on a cursor opened on a database that has been made into a secondary index, the `DBcursor->get()` and `DBcursor->pget()` methods return the record number of the primary database in **data**. In addition, the `DBcursor->pget()` method returns the record number of the secondary index in **pkey**. If either underlying database is not of type Btree or is not created with the <a href="dbset_flags.md#dbset_flags_DB_RECNUM" class="link">DB_RECNUM</a> flag, the out-of-band record number of 0 is returned.

- `DB_JOIN_ITEM`

  Do not use the data value found in all of the cursors as a lookup key for the primary database, but simply return it in the key parameter instead. The data parameter is left unchanged.

  For DB_JOIN_ITEM to be specified, the underlying cursor must have been returned from the <a href="dbjoin.md" class="xref" title="DB-&gt;join()">DB-&gt;join()</a> method.

  This flag is not supported for Heap databases.

- `DB_LAST`

  The cursor is set to refer to the last key/data pair of the database, and that pair is returned. If the last key has duplicate values, the last data item in the set of duplicates is returned.

  If the database is a Queue or Recno database, `DBcursor->get()` using the DB_LAST flag will ignore any keys that exist but were never explicitly created by the application, or were created and later deleted.

  The `DBcursor->get()` method will return <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_NOTFOUND" class="olink">DB_NOTFOUND</a> if DB_LAST is set and the database is empty.

- `DB_NEXT`

  If the cursor is not yet initialized, DB_NEXT is identical to DB_FIRST. Otherwise, the cursor is moved to the next key/data pair of the database, and that pair is returned. In the presence of duplicate key values, the value of the key may not change.

  If the database is a Queue or Recno database, `DBcursor->get()` using the DB_NEXT flag will skip any keys that exist but were never explicitly created by the application, or those that were created and later deleted.

  The `DBcursor->get()` method will return <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_NOTFOUND" class="olink">DB_NOTFOUND</a> if DB_NEXT is set and the cursor is already on the last record in the database.

- `DB_NEXT_DUP`

  If the next key/data pair of the database is a duplicate data record for the current key/data pair, the cursor is moved to the next key/data pair of the database, and that pair is returned.

  The `DBcursor->get()` method will return <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_NOTFOUND" class="olink">DB_NOTFOUND</a> if DB_NEXT_DUP is set and the next key/data pair of the database is not a duplicate data record for the current key/data pair.

  If using a Heap database, this flag results in this method returning <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_NOTFOUND" class="olink">DB_NOTFOUND</a>.

- `DB_NEXT_NODUP`

  If the cursor is not yet initialized, DB_NEXT_NODUP is identical to DB_FIRST. Otherwise, the cursor is moved to the next non-duplicate key of the database, and that key/data pair is returned.

  If the database is a Queue or Recno database, `DBcursor->get()` using the DB_NEXT_NODUP flag will ignore any keys that exist but were never explicitly created by the application, or those that were created and later deleted.

  The `DBcursor->get()` method will return <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_NOTFOUND" class="olink">DB_NOTFOUND</a> if DB_NEXT_NODUP is set and no non-duplicate key/data pairs exist after the cursor position in the database.

  If using a Heap database, this flag is identical to the `DB_NEXT` flag.

- `DB_PREV`

  If the cursor is not yet initialized, DB_PREV is identical to DB_LAST. Otherwise, the cursor is moved to the previous key/data pair of the database, and that pair is returned. In the presence of duplicate key values, the value of the key may not change.

  If the database is a Queue or Recno database, `DBcursor->get()` using the DB_PREV flag will skip any keys that exist but were never explicitly created by the application, or those that were created and later deleted.

  The `DBcursor->get()` method will return <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_NOTFOUND" class="olink">DB_NOTFOUND</a> if DB_PREV is set and the cursor is already on the first record in the database.

- `DB_PREV_DUP`

  If the previous key/data pair of the database is a duplicate data record for the current key/data pair, the cursor is moved to the previous key/data pair of the database, and that pair is returned.

  The `DBcursor->get()` method will return <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_NOTFOUND" class="olink">DB_NOTFOUND</a> if DB_PREV_DUP is set and the previous key/data pair of the database is not a duplicate data record for the current key/data pair.

  If using a Heap database, this flag results in this method returning <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_NOTFOUND" class="olink">DB_NOTFOUND</a>.

- `DB_PREV_NODUP`

  If the cursor is not yet initialized, DB_PREV_NODUP is identical to DB_LAST. Otherwise, the cursor is moved to the previous non-duplicate key of the database, and that key/data pair is returned.

  If the database is a Queue or Recno database, `DBcursor->get()` using the DB_PREV_NODUP flag will ignore any keys that exist but were never explicitly created by the application, or those that were created and later deleted.

  The `DBcursor->get()` method will return <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_NOTFOUND" class="olink">DB_NOTFOUND</a> if DB_PREV_NODUP is set and no non-duplicate key/data pairs exist before the cursor position in the database.

  If using a Heap database, this flag is identical to the `DB_PREV` flag.

- `DB_SET`

  Move the cursor to the specified key/data pair of the database, and return the datum associated with the given key.

  The `DBcursor->get()` method will return <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_NOTFOUND" class="olink">DB_NOTFOUND</a> if DB_SET is set and no matching keys are found. The `DBcursor->get()` method will return <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_KEYEMPTY" class="olink">DB_KEYEMPTY</a> if DB_SET is set and the database is a Queue or Recno database, and the specified key exists, but was never explicitly created by the application or was later deleted. In the presence of duplicate key values, `DBcursor->get()` will return the first data item for the given key.

- `DB_SET_RANGE`

  Move the cursor to the specified key/data pair of the database. In the case of the Btree access method, the key is returned as well as the data item and the returned key/data pair is the smallest key greater than or equal to the specified key (as determined by the Btree comparison function), permitting partial key matches and range searches.

  In all other ways the behavior of this flag is the same as the <a href="dbcget.md#dbcget_DB_SET" class="link">DB_SET</a> flag.

- `DB_SET_RECNO`

  Move the cursor to the specific numbered record of the database, and return the associated key/data pair. The **data** field of the specified **key** must be a pointer to a memory location from which a `db_recno_t` may be read, as described in <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a>. This memory location will be read to determine the record to be retrieved.

  For DB_SET_RECNO to be specified, the underlying database must be of type Btree, and it must have been created with the <a href="dbset_flags.md#dbset_flags_DB_RECNUM" class="link">DB_RECNUM</a> flag.

In addition, the following flags may be set by bitwise inclusively **OR**'ing them into the **flags** parameter:

- `DB_IGNORE_LEASE`

  This flag is relevant only when using a replicated environment.

  Return the data item irrespective of the state of master leases. The item will be returned under all conditions: if master leases are not configured, if the request is made to a client, if the request is made to a master with a valid lease, or if the request is made to a master without a valid lease.

- `DB_READ_COMMITTED`

  Configure a transactional get operation to have degree 2 isolation (the read is not repeatable).

- `DB_READ_UNCOMMITTED`

  Database items read during a transactional call will have degree 1 isolation, including modified but not yet committed data. Silently ignored if the <a href="dbopen.md#dbopen_DB_READ_UNCOMMITTED" class="link">DB_READ_UNCOMMITTED</a> flag was not specified when the underlying database was opened.

- `DB_MULTIPLE`

  Return multiple data items in the **data** parameter.

  In the case of Btree or Hash databases, duplicate data items for the current key, starting at the current cursor position, are entered into the buffer. Subsequent calls with both the DB_NEXT_DUP and DB_MULTIPLE flags specified will return additional duplicate data items associated with the current key or <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_NOTFOUND" class="olink">DB_NOTFOUND</a> if there are no additional duplicate data items to return. Subsequent calls with both the DB_NEXT and DB_MULTIPLE flags specified will return additional duplicate data items associated with the current key or if there are no additional duplicate data items will return the next key and its data items or <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_NOTFOUND" class="olink">DB_NOTFOUND</a> if there are no additional keys in the database.

  In the case of Queue, Recno, or Heap databases, data items starting at the current cursor position are entered into the buffer. The record number (or the RID, in the case of Heap) of the first record will be returned in the **key** parameter. For Queue and Recno, the record number of each subsequent returned record must be calculated from this value. For Heap databases, the RID of subsequent returned records cannot be known. Subsequent calls with the DB_MULTIPLE flag specified will return additional data items or <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_NOTFOUND" class="olink">DB_NOTFOUND</a> if there are no additional data items to return.

  The buffer to which the **data** parameter refers must be provided from user memory (see <a href="dbt.md#dbt_DB_DBT_USERMEM" class="link">DB_DBT_USERMEM</a> ). The buffer must be at least as large as the page size of the underlying database, aligned for unsigned integer access, and be a multiple of 1024 bytes in size. If the buffer size is insufficient, then upon return from the call the size field of the **data** parameter will have been set to an estimated buffer size, and the error DB_BUFFER_SMALL is returned. (The size is an estimate as the exact size needed may not be known until all entries are read. It is best to initially provide a relatively large buffer, but applications should be prepared to resize the buffer as necessary and repeatedly call the method.)

  The multiple data items can be iterated over using the <a href="DB_MULTIPLE_NEXT.md" class="xref" title="DB_MULTIPLE_NEXT">DB_MULTIPLE_NEXT</a> macro.

  The DB_MULTIPLE flag may only be used with the DB_CURRENT, DB_FIRST, DB_GET_BOTH, DB_GET_BOTH_RANGE, DB_NEXT, DB_NEXT_DUP, DB_NEXT_NODUP, DB_SET, DB_SET_RANGE, and DB_SET_RECNO options. The DB_MULTIPLE flag may not be used when accessing databases made into secondary indices using the <a href="dbassociate.md" class="xref" title="DB-&gt;associate()">DB-&gt;associate()</a> method.

- `DB_MULTIPLE_KEY`

  Return multiple key and data pairs in the **data** parameter.

  Key and data pairs, starting at the current cursor position, are entered into the buffer. Subsequent calls with both the DB_NEXT and DB_MULTIPLE_KEY flags specified will return additional key and data pairs or <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_NOTFOUND" class="olink">DB_NOTFOUND</a> if there are no additional key and data items to return.

  In the case of Btree, Hash or Heap databases, the multiple key and data pairs can be iterated over using the <a href="DB_MULTIPLE_KEY_NEXT.md" class="xref" title="DB_MULTIPLE_KEY_NEXT">DB_MULTIPLE_KEY_NEXT</a> macro.

  In the case of Queue or Recno databases, the multiple record number and data pairs can be iterated over using the <a href="DB_MULTIPLE_RECNO_NEXT.md" class="xref" title="DB_MULTIPLE_RECNO_NEXT">DB_MULTIPLE_RECNO_NEXT</a> macro.

  The buffer to which the **data** parameter refers must be provided from user memory (see <a href="dbt.md#dbt_DB_DBT_USERMEM" class="link">DB_DBT_USERMEM</a> ). The buffer must be at least as large as the page size of the underlying database, aligned for unsigned integer access, and be a multiple of 1024 bytes in size. If the buffer size is insufficient, then upon return from the call the size field of the **data** parameter will have been set to an estimated buffer size, and the error DB_BUFFER_SMALL is returned. (The size is an estimate as the exact size needed may not be known until all entries are read. It is best to initially provide a relatively large buffer, but applications should be prepared to resize the buffer as necessary and repeatedly call the method.)

  The DB_MULTIPLE_KEY flag may only be used with the DB_CURRENT, DB_FIRST, DB_GET_BOTH, DB_GET_BOTH_RANGE, DB_NEXT, DB_NEXT_DUP, DB_NEXT_NODUP, DB_SET, DB_SET_RANGE, and DB_SET_RECNO options. The DB_MULTIPLE_KEY flag may not be used when accessing databases made into secondary indices using the <a href="dbassociate.md" class="xref" title="DB-&gt;associate()">DB-&gt;associate()</a> method.

- `DB_RMW`

  Acquire write locks instead of read locks when doing the read, if locking is configured. Setting this flag can eliminate deadlock during a read-modify-write cycle by acquiring the write lock during the read part of the cycle so that another thread of control acquiring a read lock for the same item, in its own read-modify-write cycle, will not result in deadlock.

### Errors

The `DBcursor->get()` method may fail and return one of the following non-zero errors:

#### DB_BUFFER_SMALL

The requested item could not be returned due to undersized buffer.

#### DB_LOCK_DEADLOCK

A transactional database environment operation was selected to resolve a deadlock.

#### DB_LOCK_NOTGRANTED

A Berkeley DB Concurrent Data Store database environment configured for lock timeouts was unable to grant a lock in the allowed time.

You attempted to open a database handle that is configured for no waiting exclusive locking, but the exclusive lock could not be immediately obtained. See <a href="dbset_lk_exclusive.md" class="xref" title="DB-&gt;set_lk_exclusive()">DB-&gt;set_lk_exclusive()</a> for more information.

#### DB_REP_HANDLE_DEAD

When a client synchronizes with the master, it is possible for committed transactions to be rolled back. This invalidates all the database and cursor handles opened in the replication environment. Once this occurs, an attempt to use such a handle will return `DB_REP_HANDLE_DEAD`. The application will need to discard the handle and open a new one in order to continue processing.

#### DB_REP_LEASE_EXPIRED

The operation failed because the site's replication master lease has expired.

#### DB_REP_LOCKOUT

The operation was blocked by client/master synchronization.

#### DB_SECONDARY_BAD

A secondary index references a nonexistent primary key.

#### EINVAL

If the DB_CURRENT, DB_NEXT_DUP or DB_PREV_DUP flags were specified and the cursor has not been initialized; the `DBcursor->pget()` method was called with a cursor that does not refer to a secondary index; or if an invalid flag value or parameter was specified.

### Class

<a href="dbc.md" class="link" title="Chapter 3.  The DBcursor Handle">DBcursor</a>

### See Also

<a href="dbc.md#dbclist" class="xref" title="Database Cursors and Related Methods">Database Cursors and Related Methods</a>
