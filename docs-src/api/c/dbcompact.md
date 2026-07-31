---
title: "DB->compact()"
api-name: "DB->compact()"
source: docs/api_reference/C/dbcompact.html
---
## DB-\>compact()

``` c
#include <db.h>

int
DB->compact(DB *db, DB_TXN *txnid,
    DBT *start, DBT *stop, DB_COMPACT *c_data, u_int32_t flags, DBT *end); 
```

The `DB->compact()` method compacts Btree, Hash, and Recno access method databases, and optionally returns unused Btree, Hash or Recno database pages to the underlying filesystem.

The `DB->compact()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### txnid

If the operation is part of an application-specified transaction, the **txnid** parameter is a transaction handle returned from <a href="txnbegin.md" class="xref" title="DB_ENV-&gt;txn_begin()">DB_ENV-&gt;txn_begin()</a>; if the operation is part of a Berkeley DB Concurrent Data Store group, the **txnid** parameter is a handle returned from <a href="envcdsgroup_begin.md" class="xref" title="DB_ENV-&gt;cdsgroup_begin()">DB_ENV-&gt;cdsgroup_begin()</a>; otherwise NULL.

If a transaction handle is supplied to this method, then the operation is performed using that transaction. In this event, large sections of the tree may be locked during the course of the transaction.

If no transaction handle is specified, but the operation occurs in a transactional database, the operation will be implicitly transaction protected using multiple transactions. These transactions will be periodically committed to avoid locking large sections of the tree. Any deadlocks encountered cause the compaction operation to be retried from the point of the last transaction commit.

#### start

If non-NULL, the **start** parameter is the starting point for compaction. For a Btree or Recno database, compaction will start at the smallest key greater than or equal to the specified key. For a Hash database, the compaction will start in the bucket specified by the integer stored in the key. If NULL, compaction will start at the beginning of the database.

#### stop

If non-NULL, the **stop** parameter is the stopping point for compaction. For a Btree or Recno database, compaction will stop at the page with the smallest key greater than the specified key. For a Hash database, compaction will stop in the bucket specified by the integer stored in the key. If NULL, compaction will stop at the end of the database.

#### c_data

If non-NULL, the **c_data** parameter contains additional compaction configuration parameters, and returns compaction operation statistics, in a structure of type `DB_COMPACT`.

The following input configuration fields are available from the `DB_COMPACT` structure:

- `int compact_fillpercent;`

  If non-zero, this provides the goal for filling pages, specified as a percentage between 1 and 100. Any page in the database not at or above this percentage full will be considered for compaction. The default behavior is to consider every page for compaction, regardless of its page fill percentage.

- `int compact_pages;`

  If non-zero, the call will return after the specified number of pages have been freed, or no more pages can be freed.

- `db_timeout_t compact_timeout;`

  If non-zero, and no **txnid** parameter was specified, this parameter identifies the lock timeout used for implicit transactions, in microseconds.

The following output statistics fields are available from the `DB_COMPACT` structure:

- `u_int32_t compact_deadlock;`

  An output statistics parameter: if no **txnid** parameter was specified, the number of deadlocks which occurred.

- `u_int32_t compact_pages_examine;`

  An output statistics parameter: the number of database pages reviewed during the compaction phase.

- `u_int32_t compact_empty_buckets;`

  An output statistics parameter: the number of empty hash buckets that were found the compaction phase.

- `u_int32_t compact_pages_free;`

  An output statistics parameter: the number of database pages freed during the compaction phase.

- `u_int32_t compact_levels;`

  An output statistics parameter: the number of levels removed from the Btree or Recno database during the compaction phase.

- `u_int32_t compact_pages_truncated;`

  An output statistics parameter: the number of database pages returned to the filesystem.

#### flags

The **flags** parameter must be set to 0 or one of the following values:

- `DB_FREELIST_ONLY`

  Do no page compaction, only returning pages to the filesystem that are already free and at the end of the file.

- `DB_FREE_SPACE`

  Return pages to the filesystem when possible. If this flag is not specified, pages emptied as a result of compaction will be placed on the free list for re-use, but never returned to the filesystem.

  Note that only pages at the end of a file can be returned to the filesystem. Because of the one-pass nature of the compaction algorithm, any unemptied page near the end of the file inhibits returning pages to the file system. A repeated call to the `DB->compact()` method with a low **compact_fillpercent** may be used to return pages in this case.

#### end

If non-NULL, the **end** parameter will be filled with the database key marking the end of the compaction operation in a Btree or Recno database. This is generally the first key of the page where the operation stopped. For a Hash database, this will hold the integer value representing which bucket the compaction stopped in.

### Errors

The `DB->compact()` method may fail and return one of the following non-zero errors:

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

An invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
