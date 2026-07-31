---
title: "DB->set_flags()"
api-name: "DB->set_flags()"
source: docs/api_reference/C/dbset_flags.html
---
## DB-\>set_flags()

``` c
#include <db.h>

int
DB->set_flags(DB *db, u_int32_t flags);  
```

Configure a database. Calling `DB->set_flags()` is additive; there is no way to clear flags.

The `DB->set_flags()` method may not be called after the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called.

The `DB->set_flags()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flags

The **flags** parameter must be set to 0 or by bitwise inclusively **OR**'ing together one or more of the following values:

**General**

The following flags may be specified for any Berkeley DB access method:

- `DB_CHKSUM`

  Do checksum verification of pages read into the cache from the backing filestore. Berkeley DB uses the SHA1 Secure Hash Algorithm if encryption is configured and a general hash algorithm if it is not.

  Calling `DB->set_flags()` with the DB_CHKSUM flag only affects the specified <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle (and any other Berkeley DB handles opened within the scope of that handle).

  If the database already exists when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called, the DB_CHKSUM flag will be ignored.

- `DB_ENCRYPT`

  Encrypt the database using the cryptographic password specified to the <a href="envset_encrypt.md" class="xref" title="DB_ENV-&gt;set_encrypt()">DB_ENV-&gt;set_encrypt()</a> or <a href="dbset_encrypt.md" class="xref" title="DB-&gt;set_encrypt()">DB-&gt;set_encrypt()</a> methods.

  Calling `DB->set_flags()` with the DB_ENCRYPT flag only affects the specified <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle (and any other Berkeley DB handles opened within the scope of that handle).

  If the database already exists when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called, the DB_ENCRYPT flag must be the same as the existing database or an error will be returned.

  Encrypted databases are not portable between machines of different byte orders, that is, encrypted databases created on big-endian machines cannot be read on little-endian machines, and vice versa.

- `DB_TXN_NOT_DURABLE`

  If set, Berkeley DB will not write log records for this database. This means that updates of this database exhibit the ACI (atomicity, consistency, and isolation) properties, but not D (durability); that is, database integrity will be maintained, but if the application or system fails, integrity will not persist. The database file must be verified and/or restored from backup after a failure. In order to ensure integrity after application shut down, the database handles must be closed without specifying <a href="dbclose.md#dbclose_DB_NOSYNC" class="link">DB_NOSYNC</a>, or all database changes must be flushed from the database environment cache using either the <a href="txncheckpoint.md" class="xref" title="DB_ENV-&gt;txn_checkpoint()">DB_ENV-&gt;txn_checkpoint()</a> or <a href="mempsync.md" class="xref" title="DB_ENV-&gt;memp_sync()">DB_ENV-&gt;memp_sync()</a> methods. All database handles for a single physical file must set DB_TXN_NOT_DURABLE, including database handles for different databases in a physical file.

  Calling `DB->set_flags()` with the DB_TXN_NOT_DURABLE flag only affects the specified <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle (and any other Berkeley DB handles opened within the scope of that handle).

**Btree**

The following flags may be specified for the Btree access method:

- `DB_DUP`

  Permit duplicate data items in the database; that is, insertion when the key of the key/data pair being inserted already exists in the database will be successful. The ordering of duplicates in the database is determined by the order of insertion, unless the ordering is otherwise specified by use of a cursor operation or a duplicate sort function.

  The DB_DUPSORT flag is preferred to DB_DUP for performance reasons. The DB_DUP flag should only be used by applications wanting to order duplicate data items manually.

  Calling `DB->set_flags()` with the DB_DUP flag affects the database, including all threads of control accessing the database.

  If the database already exists when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called, the DB_DUP flag must be the same as the existing database or an error will be returned.

  It is an error to specify both DB_DUP and DB_RECNUM.

- `DB_DUPSORT`

  Permit duplicate data items in the database; that is, insertion when the key of the key/data pair being inserted already exists in the database will be successful. The ordering of duplicates in the database is determined by the duplicate comparison function. If the application does not specify a comparison function using the <a href="dbset_dup_compare.md" class="xref" title="DB-&gt;set_dup_compare()">DB-&gt;set_dup_compare()</a> method, a default lexical comparison will be used. It is an error to specify both DB_DUPSORT and DB_RECNUM.

  Calling `DB->set_flags()` with the DB_DUPSORT flag affects the database, including all threads of control accessing the database.

  If the database already exists when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called, the DB_DUPSORT flag must be the same as the existing database or an error will be returned.

- `DB_RECNUM`

  Support retrieval from the Btree using record numbers. For more information, see the <a href="dbget.md#dbget_DB_SET_RECNO" class="link">DB_SET_RECNO</a> flag to the <a href="dbget.md" class="xref" title="DB-&gt;get()">DB-&gt;get()</a> and <a href="dbcget.md" class="xref" title="DBcursor-&gt;get()">DBcursor-&gt;get()</a> methods.

  Logical record numbers in Btree databases are mutable in the face of record insertion or deletion. See the DB_RENUMBER flag in the Recno access method information for further discussion.

  Maintaining record counts within a Btree introduces a serious point of contention, namely the page locations where the record counts are stored. In addition, the entire database must be locked during both insertions and deletions, effectively single-threading the database for those operations. Specifying DB_RECNUM can result in serious performance degradation for some applications and data sets.

  It is an error to specify both DB_DUP and DB_RECNUM.

  Calling `DB->set_flags()` with the DB_RECNUM flag affects the database, including all threads of control accessing the database.

  If the database already exists when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called, the DB_RECNUM flag must be the same as the existing database or an error will be returned.

- `DB_REVSPLITOFF`

  Turn off reverse splitting in the Btree. As pages are emptied in a database, the Berkeley DB Btree implementation attempts to coalesce empty pages into higher-level pages in order to keep the database as small as possible and minimize search time. This can hurt performance in applications with cyclical data demands; that is, applications where the database grows and shrinks repeatedly. For example, because Berkeley DB does page-level locking, the maximum level of concurrency in a database of two pages is far smaller than that in a database of 100 pages, so a database that has shrunk to a minimal size can cause severe deadlocking when a new cycle of data insertion begins.

  Calling `DB->set_flags()` with the DB_REVSPLITOFF flag only affects the specified <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle (and any other Berkeley DB handles opened within the scope of that handle).

**Hash**

The following flags may be specified for the Hash access method:

- `DB_DUP`

  Permit duplicate data items in the database; that is, insertion when the key of the key/data pair being inserted already exists in the database will be successful. The ordering of duplicates in the database is determined by the order of insertion, unless the ordering is otherwise specified by use of a cursor operation.

  The DB_DUPSORT flag is preferred to DB_DUP for performance reasons. The DB_DUP flag should only be used by applications wanting to order duplicate data items manually.

  Calling `DB->set_flags()` with the DB_DUP flag affects the database, including all threads of control accessing the database.

  If the database already exists when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called, the DB_DUP flag must be the same as the existing database or an error will be returned.

- `DB_DUPSORT`

  Permit duplicate data items in the database; that is, insertion when the key of the key/data pair being inserted already exists in the database will be successful. The ordering of duplicates in the database is determined by the duplicate comparison function. If the application does not specify a comparison function using the <a href="dbset_dup_compare.md" class="xref" title="DB-&gt;set_dup_compare()">DB-&gt;set_dup_compare()</a> method, a default lexical comparison will be used.

  Calling `DB->set_flags()` with the DB_DUPSORT flag affects the database, including all threads of control accessing the database.

  If the database already exists when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called, the DB_DUPSORT flag must be the same as the existing database or an error will be returned.

- `DB_REVSPLITOFF`

  Turns off hash bucket compaction. When a hash bucket is emptied, the Berkeley DB Hash implementation will decrease the hash table size, coalescing buckets. This will decrease the number of pages in the database. This can hurt performance in applications with cyclical data demands — that is, applications where the database grows and shrinks repeatedly — because of the cost of resplitting buckets when they grow again.

  Calling `DB->set_flags()` with the DB_REVSPLITOFF flag only affects the specified <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle (and any other Berkeley DB handles opened within the scope of that handle).

**Queue**

The following flags may be specified for the Queue access method:

- `DB_INORDER`

  The DB_INORDER flag modifies the operation of the <a href="dbget.md#dbget_DB_CONSUME" class="link">DB_CONSUME</a> or <a href="dbget.md#dbget_DB_CONSUME_WAIT" class="link">DB_CONSUME_WAIT</a> flags to <a href="dbget.md" class="xref" title="DB-&gt;get()">DB-&gt;get()</a> to return key/data pairs in order. That is, they will always return the key/data item from the head of the queue.

  The default behavior of queue databases is optimized for multiple readers, and does not guarantee that record will be retrieved in the order they are added to the queue. Specifically, if a writing thread adds multiple records to an empty queue, reading threads may skip some of the initial records when the next <a href="dbget.md" class="xref" title="DB-&gt;get()">DB-&gt;get()</a> call returns.

  This flag modifies the <a href="dbget.md" class="xref" title="DB-&gt;get()">DB-&gt;get()</a> call to verify that the record being returned is in fact the head of the queue. This will increase contention and reduce concurrency when there are many reading threads.

  Calling `DB->set_flags()` with the DB_INORDER flag only affects the specified <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle (and any other Berkeley DB handles opened within the scope of that handle).

**Recno**

The following flags may be specified for the Recno access method:

- `DB_RENUMBER`

  Specifying the DB_RENUMBER flag causes the logical record numbers to be mutable, and change as records are added to and deleted from the database.

  Using the <a href="dbput.md" class="xref" title="DB-&gt;put()">DB-&gt;put()</a> or <a href="dbcput.md" class="xref" title="DBcursor-&gt;put()">DBcursor-&gt;put()</a> interfaces to create new records will cause the creation of multiple records if the record number is more than one greater than the largest record currently in the database. For example, creating record 28, when record 25 was previously the last record in the database, will create records 26 and 27 as well as 28. Attempts to retrieve records that were created in this manner will result in an error return of <a href="../../guides/programmer_reference/program_errorret.md#program_errorret.DB_KEYEMPTY" class="olink">DB_KEYEMPTY</a>.

  If a created record is not at the end of the database, all records following the new record will be automatically renumbered upward by one. For example, the creation of a new record numbered 8 causes records numbered 8 and greater to be renumbered upward by one. If a cursor was positioned to record number 8 or greater before the insertion, it will be shifted upward one logical record, continuing to refer to the same record as it did before.

  If a deleted record is not at the end of the database, all records following the removed record will be automatically renumbered downward by one. For example, deleting the record numbered 8 causes records numbered 9 and greater to be renumbered downward by one. If a cursor was positioned to record number 9 or greater before the removal, it will be shifted downward one logical record, continuing to refer to the same record as it did before.

  If a record is deleted, all cursors that were positioned on that record prior to the removal will no longer be positioned on a valid entry. This includes cursors used to delete an item. For example, if a cursor was positioned to record number 8 before the removal of that record, subsequent calls to <a href="dbcget.md" class="xref" title="DBcursor-&gt;get()">DBcursor-&gt;get()</a> with flags of DB_CURRENT will result in an error return of <a href="../../guides/programmer_reference/program_errorret.md#program_errorret.DB_KEYEMPTY" class="olink">DB_KEYEMPTY</a> until the cursor is moved to another record. A call to <a href="dbcget.md" class="xref" title="DBcursor-&gt;get()">DBcursor-&gt;get()</a> with flags of DB_NEXT will return the new record numbered 8 - which is the record that was numbered 9 prior to the delete (if such a record existed).

  For these reasons, concurrent access to a Recno database with the DB_RENUMBER flag specified may be largely meaningless, although it is supported.

  Calling `DB->set_flags()` with the DB_RENUMBER flag affects the database, including all threads of control accessing the database.

  If the database already exists when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called, the DB_RENUMBER flag must be the same as the existing database or an error will be returned.

- `DB_SNAPSHOT`

  This flag specifies that any specified **re_source** file be read in its entirety when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called. If this flag is not specified, the **re_source** file may be read lazily.

  See the <a href="dbset_re_source.md" class="xref" title="DB-&gt;set_re_source()">DB-&gt;set_re_source()</a> method for information on the **re_source** file.

  Calling `DB->set_flags()` with the DB_SNAPSHOT flag only affects the specified <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle (and any other Berkeley DB handles opened within the scope of that handle).

### Errors

The `DB->set_flags()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
