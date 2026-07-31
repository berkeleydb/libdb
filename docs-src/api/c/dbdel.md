---
title: "DB->del()"
api-name: "DB->del()"
source: docs/api_reference/C/dbdel.html
---
## DB-\>del()

``` c
#include <db.h>

int
DB->del(DB *db, DB_TXN *txnid, DBT *key, u_int32_t flags);  
```

The `DB->del()` method removes key/data pairs from the database. The key/data pair associated with the specified **key** is discarded from the database. In the presence of duplicate key values, all records associated with the designated key will be discarded.

When called on a database that has been made into a secondary index using the <a href="dbassociate.md" class="xref" title="DB-&gt;associate()">DB-&gt;associate()</a> method, the `DB->del()` method deletes the key/data pair from the primary database and all secondary indices.

The `DB->del()` method will return <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_NOTFOUND" class="olink">DB_NOTFOUND</a> if the specified key is not in the database. The `DB->del()` method will return <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_KEYEMPTY" class="olink">DB_KEYEMPTY</a> if the database is a Queue or Recno database and the specified key exists, but was never explicitly created by the application or was later deleted. Unless otherwise specified, the `DB->del()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### txnid

If the operation is part of an application-specified transaction, the **txnid** parameter is a transaction handle returned from <a href="txnbegin.md" class="xref" title="DB_ENV-&gt;txn_begin()">DB_ENV-&gt;txn_begin()</a>; if the operation is part of a Berkeley DB Concurrent Data Store group, the **txnid** parameter is a handle returned from <a href="envcdsgroup_begin.md" class="xref" title="DB_ENV-&gt;cdsgroup_begin()">DB_ENV-&gt;cdsgroup_begin()</a>; otherwise NULL. If no transaction handle is specified, but the operation occurs in a transactional database, the operation will be implicitly transaction protected.

#### key

The key <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> operated on.

#### flags

The **flags** parameter must be set to 0 or one of the following values:

- `DB_CONSUME`

  If the database is of type DB_QUEUE then this flag may be set to force the head of the queue to move to the first non-deleted item in the queue. Normally this is only done if the deleted item is exactly at the head when deleted.

- `DB_MULTIPLE`

  Delete multiple data items using keys from the buffer to which the **key** parameter refers.

  To delete records in bulk by key with the btree or hash access methods, construct a bulk buffer in the **key** <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> using <a href="DB_MULTIPLE_WRITE_INIT.md" class="xref" title="DB_MULTIPLE_WRITE_INIT">DB_MULTIPLE_WRITE_INIT</a> and <a href="DB_MULTIPLE_WRITE_NEXT.md" class="xref" title="DB_MULTIPLE_WRITE_NEXT">DB_MULTIPLE_WRITE_NEXT</a>. To delete records in bulk by record number, construct the **key** <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> using <a href="DB_MULTIPLE_RECNO_WRITE_INIT.md" class="xref" title="DB_MULTIPLE_RECNO_WRITE_INIT">DB_MULTIPLE_RECNO_WRITE_INIT</a> and <a href="DB_MULTIPLE_RECNO_WRITE_NEXT.md" class="xref" title="DB_MULTIPLE_RECNO_WRITE_NEXT">DB_MULTIPLE_RECNO_WRITE_NEXT</a> with a data size of zero.

  A successful bulk delete operation is logically equivalent to a loop through each key/data pair, performing a <a href="dbdel.md" class="xref" title="DB-&gt;del()">DB-&gt;del()</a> for each one.

  See the <a href="dbt.md#dbtlist" class="xref" title="DBT and Bulk Operations">DBT and Bulk Operations</a> for more information on working with bulk updates.

  The `DB_MULTIPLE` flag may only be used alone.

- `DB_MULTIPLE_KEY`

  Delete multiple data items using keys and data from the buffer to which the **key** parameter refers.

  To delete records in bulk with the btree or hash access methods, construct a bulk buffer in the **key** <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> using <a href="DB_MULTIPLE_WRITE_INIT.md" class="xref" title="DB_MULTIPLE_WRITE_INIT">DB_MULTIPLE_WRITE_INIT</a> and <a href="DB_MULTIPLE_KEY_WRITE_NEXT.md" class="xref" title="DB_MULTIPLE_KEY_WRITE_NEXT">DB_MULTIPLE_KEY_WRITE_NEXT</a>. To delete records in bulk with the recno or hash access methods, construct a bulk buffer in the **key** <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> using <a href="DB_MULTIPLE_RECNO_WRITE_INIT.md" class="xref" title="DB_MULTIPLE_RECNO_WRITE_INIT">DB_MULTIPLE_RECNO_WRITE_INIT</a> and <a href="DB_MULTIPLE_RECNO_WRITE_NEXT.md" class="xref" title="DB_MULTIPLE_RECNO_WRITE_NEXT">DB_MULTIPLE_RECNO_WRITE_NEXT</a>.

  See the <a href="dbt.md#dbtlist" class="xref" title="DBT and Bulk Operations">DBT and Bulk Operations</a> for more information on working with bulk updates.

  The `DB_MULTIPLE_KEY` flag may only be used alone.

### Errors

The `DB->del()` method may fail and return one of the following non-zero errors:

#### DB_FOREIGN_CONFLICT

A <a href="dbassociate_foreign.md" class="link" title="DB-&gt;associate_foreign()">foreign key constraint violation</a> has occurred. This can be caused by one of two things:

1.  An attempt was made to add a record to a constrained database, and the key used for that record does not exist in the foreign key database.

2.  <a href="dbassociate_foreign.md#associate_foreign_DB_FOREIGN_ABORT" class="link">DB_FOREIGN_ABORT</a> was declared for a foreign key database, and then subsequently a record was deleted from the foreign key database without first removing it from the constrained secondary database.

#### DB_LOCK_DEADLOCK

A transactional database environment operation was selected to resolve a deadlock.

#### DB_LOCK_NOTGRANTED

A Berkeley DB Concurrent Data Store database environment configured for lock timeouts was unable to grant a lock in the allowed time.

You attempted to open a database handle that is configured for no waiting exclusive locking, but the exclusive lock could not be immediately obtained. See <a href="dbset_lk_exclusive.md" class="xref" title="DB-&gt;set_lk_exclusive()">DB-&gt;set_lk_exclusive()</a> for more information.

#### DB_REP_HANDLE_DEAD

When a client synchronizes with the master, it is possible for committed transactions to be rolled back. This invalidates all the database and cursor handles opened in the replication environment. Once this occurs, an attempt to use such a handle will return `DB_REP_HANDLE_DEAD`. The application will need to discard the handle and open a new one in order to continue processing.

#### DB_REP_LOCKOUT

The operation was blocked by client/master synchronization.

#### DB_SECONDARY_BAD

A secondary index references a nonexistent primary key.

#### EACCES

An attempt was made to modify a read-only database.

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
