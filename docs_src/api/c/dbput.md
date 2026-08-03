---
title: "DB->put()"
api-name: "DB->put()"
source: docs/api_reference/C/dbput.html
---
## DB-\>put()

``` c
#include <db.h>

int
DB->put(DB *db,
    DB_TXN *txnid, DBT *key, DBT *data, u_int32_t flags);  
```

The `DB->put()` method stores key/data pairs in the database. The default behavior of the `DB->put()` function is to enter the new key/data pair, replacing any previously existing key if duplicates are disallowed, or adding a duplicate data item if duplicates are allowed. If the database supports duplicates, the `DB->put()` method adds the new data value at the end of the duplicate set. If the database supports sorted duplicates, the new data value is inserted at the correct sorted location.

Unless otherwise specified, the `DB->put()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### txnid

If the operation is part of an application-specified transaction, the **txnid** parameter is a transaction handle returned from <a href="txnbegin.md" class="xref" title="DB_ENV-&gt;txn_begin()">DB_ENV-&gt;txn_begin()</a>; if the operation is part of a Berkeley DB Concurrent Data Store group, the **txnid** parameter is a handle returned from <a href="envcdsgroup_begin.md" class="xref" title="DB_ENV-&gt;cdsgroup_begin()">DB_ENV-&gt;cdsgroup_begin()</a>; otherwise NULL. If no transaction handle is specified, but the operation occurs in a transactional database, the operation will be implicitly transaction protected.

#### key

The key <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> operated on.

If creating a new record in a Heap database, the key <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> must be empty. The `put` method will return the new record's <a href="db_heap_rid.md" class="link" title="DB_HEAP_RID">Record ID (RID)</a> in the key <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a>.

#### data

The data <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> operated on.

#### flags

The **flags** parameter must be set to 0 or one of the following values:

- `DB_APPEND`

  Append the key/data pair to the end of the database. For the DB_APPEND flag to be specified, the underlying database must be a Heap, Queue or Recno database. The record number allocated to the record is returned in the specified **key**.

  There is a minor behavioral difference between the Recno and Queue access methods for the DB_APPEND flag. If a transaction enclosing a `DB->put()` operation with the DB_APPEND flag aborts, the record number may be reallocated in a subsequent `DB_APPEND` operation if you are using the Recno access method, but it will not be reallocated if you are using the Queue access method.

  For a Heap database, if the put operation results in the creation of a new record, then this flag is required.

- `DB_NODUPDATA`

  In the case of the Btree and Hash access methods, enter the new key/data pair only if it does not already appear in the database.

  The DB_NODUPDATA flag may only be specified if the underlying database has been configured to support sorted duplicates. The DB_NODUPDATA flag may not be specified to the Queue or Recno access methods.

  The `DB->put()` method will return <a href="dbcput.md#dbcput_DB_KEYEXIST" class="xref" title="DB_KEYEXIST">DB_KEYEXIST</a> if DB_NODUPDATA is set and the key/data pair already appears in the database.

- `DB_NOOVERWRITE`

  Enter the new key/data pair only if the key does not already appear in the database. The `DB->put()` method call with the DB_NOOVERWRITE flag set will fail if the key already exists in the database, even if the database supports duplicates.

  The `DB->put()` method will return <a href="dbcput.md#dbcput_DB_KEYEXIST" class="xref" title="DB_KEYEXIST">DB_KEYEXIST</a> if DB_NOOVERWRITE is set and the key already appears in the database.

  This enforcement of uniqueness of keys applies only to the primary key. The behavior of insertions into secondary databases is not affected by the DB_NOOVERWRITE flag. In particular, the insertion of a record that would result in the creation of a duplicate key in a secondary database that allows duplicates would not be prevented by the use of this flag.

- `DB_MULTIPLE`

  Put multiple data items using keys from the buffer to which the **key** parameter refers and data values from the buffer to which the **data** parameter refers.

  To put records in bulk with the btree or hash access methods, construct bulk buffers in the **key** and **data** <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> using <a href="DB_MULTIPLE_WRITE_INIT.md" class="xref" title="DB_MULTIPLE_WRITE_INIT">DB_MULTIPLE_WRITE_INIT</a> and <a href="DB_MULTIPLE_WRITE_NEXT.md" class="xref" title="DB_MULTIPLE_WRITE_NEXT">DB_MULTIPLE_WRITE_NEXT</a>. To put records in bulk with the recno or queue access methods, construct bulk buffers in the **data** <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> as before, but construct the **key** <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> using <a href="DB_MULTIPLE_RECNO_WRITE_INIT.md" class="xref" title="DB_MULTIPLE_RECNO_WRITE_INIT">DB_MULTIPLE_RECNO_WRITE_INIT</a> and <a href="DB_MULTIPLE_RECNO_WRITE_NEXT.md" class="xref" title="DB_MULTIPLE_RECNO_WRITE_NEXT">DB_MULTIPLE_RECNO_WRITE_NEXT</a> with a data size of zero.

  A successful bulk operation is logically equivalent to a loop through each key/data pair, performing a <a href="dbput.md" class="xref" title="DB-&gt;put()">DB-&gt;put()</a> for each one.

  See <a href="dbt.md#dbtlist" class="xref" title="DBT and Bulk Operations">DBT and Bulk Operations</a> for more information on working with bulk updates.

  The `DB_MULTIPLE` flag may only be used alone, or with the `DB_OVERWRITE_DUP` option.

- `DB_MULTIPLE_KEY`

  Put multiple data items using keys and data from the buffer to which the **key** parameter refers.

  To put records in bulk with the btree or hash access methods, construct a single bulk buffer in the **key** <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> using <a href="DB_MULTIPLE_WRITE_INIT.md" class="xref" title="DB_MULTIPLE_WRITE_INIT">DB_MULTIPLE_WRITE_INIT</a> and <a href="DB_MULTIPLE_KEY_WRITE_NEXT.md" class="xref" title="DB_MULTIPLE_KEY_WRITE_NEXT">DB_MULTIPLE_KEY_WRITE_NEXT</a>. To put records in bulk with the recno or queue access methods, construct a bulk buffer in the **key** <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> using <a href="DB_MULTIPLE_RECNO_WRITE_INIT.md" class="xref" title="DB_MULTIPLE_RECNO_WRITE_INIT">DB_MULTIPLE_RECNO_WRITE_INIT</a> and <a href="DB_MULTIPLE_RECNO_WRITE_NEXT.md" class="xref" title="DB_MULTIPLE_RECNO_WRITE_NEXT">DB_MULTIPLE_RECNO_WRITE_NEXT</a>.

  See <a href="dbt.md#dbtlist" class="xref" title="DBT and Bulk Operations">DBT and Bulk Operations</a> for more information on working with bulk updates.

  The `DB_MULTIPLE_KEY` flag may only be used alone, or with the `DB_OVERWRITE_DUP` option.

- `DB_OVERWRITE_DUP`

  Ignore duplicate records when overwriting records in a database configured for sorted duplicates.

  Normally, if a database is configured for sorted duplicates, an attempt to put a record that compares identically to a record already existing in the database will fail. Using this flag causes the put to silently proceed, without failure.

  This flag is extremely useful when performing bulk puts (using the `DB_MULTIPLE` or `DB_MULTIPLE_KEY` flags). Depending on the number of records you are writing to the database with a bulk put, you may not want the operation to fail in the event that a duplicate record is encountered. Using this flag along with the `DB_MULTIPLE` or `DB_MULTIPLE_KEY` flags allows the bulk put to complete, even if a duplicate record is encountered.

  This flag is also useful if you are using a custom comparison function that compares only part of the data portion of a record. In this case, two records can compare equally when, in fact, they are not equal. This flag allows the put to complete, even if your custom comparison routine claims the two records are equal.

### Errors

The `DB->put()` method may fail and return one of the following non-zero errors:

#### DB_FOREIGN_CONFLICT

A <a href="dbassociate_foreign.md" class="link" title="DB-&gt;associate_foreign()">foreign key constraint violation</a> has occurred. This can be caused by one of two things:

1.  An attempt was made to add a record to a constrained database, and the key used for that record does not exist in the foreign key database.

2.  <a href="dbassociate_foreign.md#associate_foreign_DB_FOREIGN_ABORT" class="link">DB_FOREIGN_ABORT</a> was declared for a foreign key database, and then subsequently a record was deleted from the foreign key database without first removing it from the constrained secondary database.

#### DB_HEAP_FULL

An attempt was made to add or update a record in a Heap database. However, the size of the database was constrained using the <a href="dbset_heapsize.md" class="xref" title="DB-&gt;set_heapsize()">DB-&gt;set_heapsize()</a> method, and that limit has been reached.

#### DB_LOCK_DEADLOCK

A transactional database environment operation was selected to resolve a deadlock.

#### DB_LOCK_NOTGRANTED

A Berkeley DB Concurrent Data Store database environment configured for lock timeouts was unable to grant a lock in the allowed time.

You attempted to open a database handle that is configured for no waiting exclusive locking, but the exclusive lock could not be immediately obtained. See <a href="dbset_lk_exclusive.md" class="xref" title="DB-&gt;set_lk_exclusive()">DB-&gt;set_lk_exclusive()</a> for more information.

#### DB_REP_HANDLE_DEAD

When a client synchronizes with the master, it is possible for committed transactions to be rolled back. This invalidates all the database and cursor handles opened in the replication environment. Once this occurs, an attempt to use such a handle will return `DB_REP_HANDLE_DEAD`. The application will need to discard the handle and open a new one in order to continue processing.

#### DB_REP_LOCKOUT

The operation was blocked by client/master synchronization.

#### EACCES

An attempt was made to modify a read-only database.

#### EINVAL

If a record number of 0 was specified; an attempt was made to add a record to a fixed-length database that was too large to fit; an attempt was made to do a partial put; an attempt was made to add a record to a secondary index; or if an invalid flag value or parameter was specified.

#### ENOSPC

A btree exceeded the maximum btree depth (255).

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
