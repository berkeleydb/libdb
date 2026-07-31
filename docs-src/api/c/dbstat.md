---
title: "DB->stat()"
api-name: "DB->stat()"
source: docs/api_reference/C/dbstat.html
---
## DB-\>stat()

``` c
#include <db.h>

int
DB->stat(DB *db, DB_TXN *txnid, void *sp, u_int32_t flags);  
```

The `DB->stat()` method creates a statistical structure and copies a pointer to it into user-specified memory locations. Specifically, if **sp** is non-NULL, a pointer to the statistics for the database are copied into the memory location to which it refers.

The `DB->stat()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### txnid

If the operation is part of an application-specified transaction, the **txnid** parameter is a transaction handle returned from <a href="txnbegin.md" class="xref" title="DB_ENV-&gt;txn_begin()">DB_ENV-&gt;txn_begin()</a>; if the operation is part of a Berkeley DB Concurrent Data Store group, the **txnid** parameter is a handle returned from <a href="envcdsgroup_begin.md" class="xref" title="DB_ENV-&gt;cdsgroup_begin()">DB_ENV-&gt;cdsgroup_begin()</a>; otherwise NULL. If no transaction handle is specified, but the operation occurs in a transactional database, the operation will be implicitly transaction protected.

#### flags

The **flags** parameter must be set to 0 or one of the following values:

- `DB_FAST_STAT`

  Return only the values which do not require traversal of the database. Among other things, this flag makes it possible for applications to request key and record counts without incurring the performance penalty of traversing the entire database.

- `DB_READ_COMMITTED`

  Database items read during a transactional call will have degree 2 isolation. This ensures the stability of the data items read during the stat operation but permits that data to be modified or deleted by other transactions prior to the commit of the specified transaction.

- `DB_READ_UNCOMMITTED`

  Database items read during a transactional call will have degree 1 isolation, including modified but not yet committed data. Silently ignored if the <a href="dbopen.md#dbopen_DB_READ_UNCOMMITTED" class="link">DB_READ_UNCOMMITTED</a> flag was not specified when the underlying database was opened.

### Statistical Structure

Statistical structures are stored in allocated memory. If application-specific allocation routines have been declared (see <a href="envset_alloc.md" class="xref" title="DB_ENV-&gt;set_alloc()">DB_ENV-&gt;set_alloc()</a> for more information), they are used to allocate the memory; otherwise, the standard C library **malloc**(3) is used. The caller is responsible for deallocating the memory. To deallocate the memory, free the memory reference; references inside the returned memory need not be individually freed.

If the DB_FAST_STAT flag has not been specified, the `DB->stat()` method will access some of or all the pages in the database, incurring a severe performance penalty as well as possibly flushing the underlying buffer pool.

In the presence of multiple threads or processes accessing an active database, the information returned by DB-\>stat may be out-of-date.

If the database was not opened read-only and the DB_FAST_STAT flag was not specified, the cached key and record numbers will be updated after the statistical information has been gathered.

The `DB->stat()` method may not be called before the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called.

The `DB->stat()` method returns a non-zero error value on failure and 0 on success.

#### Hash Statistics

In the case of a Hash database, the statistics are stored in a structure of type `DB_HASH_STAT`. The following fields will be filled in:

- `uintmax_t hash_bfree;`

  The number of bytes free on bucket pages.

- `u_int32_t hash_bigpages;`

  The number of big key/data pages.

- `uintmax_t hash_big_bfree;`

  The number of bytes free on big item pages.

- `u_int32_t hash_buckets;`

  The number of hash buckets. Returned if DB_FAST_STAT is set.

- `u_int32_t hash_dup;`

  The number of duplicate pages.

- `uintmax_t hash_dup_free;`

  The number of bytes free on duplicate pages.

- `u_int32_t hash_ffactor;`

  The desired fill factor (number of items per bucket) specified at database-creation time. Returned if DB_FAST_STAT is set.

- `u_int32_t hash_free;`

  The number of pages on the free list.

- `u_int32_t hash_magic;`

  Magic number that identifies the file as a Hash file. Returned if DB_FAST_STAT is set.

- `u_int32_t hash_metaflags;`

  Reports internal flags. For internal use only.

- `u_int32_t hash_ndata;`

  The number of key/data pairs in the database. If DB_FAST_STAT was specified the count will be the last saved value unless it has never been calculated, in which case it will be 0. Returned if DB_FAST_STAT is set.

- `u_int32_t hash_nkeys;`

  The number of unique keys in the database. If DB_FAST_STAT was specified the count will be the last saved value unless it has never been calculated, in which case it will be 0. Returned if DB_FAST_STAT is set.

- `u_int32_t hash_overflows;`

  The number of overflow pages (overflow pages are pages that contain items that did not fit in the main bucket page).

- `uintmax_t hash_ovfl_free;`

  The number of bytes free on overflow pages.

- `u_int32_t hash_pagecnt;`

  The number of pages in the database. Returned if DB_FAST_STAT is set.

- `u_int32_t hash_pagesize;`

  The underlying database page (and bucket) size, in bytes. Returned if DB_FAST_STAT is set.

- `u_int32_t hash_version;`

  The version of the Hash database. Returned if DB_FAST_STAT is set.

#### Heap Statistics

In the case of a Heap database, the statistics are stored in a structure of type `DB_HEAP_STAT`. The following fields will be filled in:

- `u_int32_t heap_magic;`

  Magic number that identifies the file as a Heap file. Returned if DB_FAST_STAT is set.

- `u_int32_t heap_version;`

  The version of the Heap database. Returned if DB_FAST_STAT is set.

- `u_int32_t heap_metaflags;`

  Reports internal flags. For internal use only.

- `u_int32_t heap_nrecs;`

  Reports the number of records in the Heap database. Returned if DB_FAST_STAT is set.

- `u_int32_t heap_pagecnt;`

  The number of pages in the database. Returned if DB_FAST_STAT is set.

- `u_int32_t heap_pagesize;`

  The underlying database page (and bucket) size, in bytes. Returned if DB_FAST_STAT is set.

- `u_int32_t heap_nregions;`

  The number of regions in the Heap database. Returned if DB_FAST_STAT is set.

- `u_int32_t heap_regionsize;`

  The number of pages in a region in the Heap database. Returned if DB_FAST_STAT is set.

#### Btree and Recno Statistics

In the case of a Btree or Recno database, the statistics are stored in a structure of type `DB_BTREE_STAT`. The following fields will be filled in:

- `u_int32_t bt_dup_pg;`

  Number of database duplicate pages.

- `uintmax_t bt_dup_pgfree;`

  Number of bytes free in database duplicate pages.

- `u_int32_t bt_empty_pg;`

  Number of empty database pages.

- `u_int32_t bt_free;`

  Number of pages on the free list.

- `u_int32_t bt_int_pg;`

  Number of database internal pages.

- `uintmax_t bt_int_pgfree;`

  Number of bytes free in database internal pages.

- `u_int32_t bt_leaf_pg;`

  Number of database leaf pages.

- `uintmax_t bt_leaf_pgfree;`

  Number of bytes free in database leaf pages.

- `u_int32_t bt_levels;`

  Number of levels in the database.

- `u_int32_t bt_magic;`

  Magic number that identifies the file as a Btree database. Returned if DB_FAST_STAT is set.

- `u_int32_t bt_metaflags;`

  Reports internal flags. For internal use only.

- `u_int32_t bt_minkey;`

  The minimum keys per page. Returned if DB_FAST_STAT is set.

- `u_int32_t bt_ndata;`

  For the Btree Access Method, the number of key/data pairs in the database. If the DB_FAST_STAT flag is not specified, the count will be exact. Otherwise, the count will be the last saved value unless it has never been calculated, in which case it will be 0.

  For the Recno Access Method, the number of records in the database. If the database was configured with mutable record numbers (see <a href="dbset_flags.md#dbset_flags_DB_RENUMBER" class="link">DB_RENUMBER</a>), the count will be exact. Otherwise, if the DB_FAST_STAT flag is specified the count will be exact but will include deleted and implicitly created records; if the DB_FAST_STAT flag is not specified, the count will be exact and will not include deleted or implicitly created records.

  Returned if DB_FAST_STAT is set.

- `u_int32_t bt_nkeys;`

  For the Btree Access Method, the number of keys in the database. If the DB_FAST_STAT flag is not specified or the database was configured to support record numbers (see <a href="dbset_flags.md#dbset_flags_DB_RECNUM" class="link">DB_RECNUM</a>), the count will be exact. Otherwise, the count will be the last saved value unless it has never been calculated, in which case it will be 0.

  For the Recno Access Method, the number of records in the database. If the database was configured with mutable record numbers (see <a href="dbset_flags.md#dbset_flags_DB_RENUMBER" class="link">DB_RENUMBER</a>), the count will be exact. Otherwise, if the DB_FAST_STAT flag is specified the count will be exact but will include deleted and implicitly created records; if the DB_FAST_STAT flag is not specified, the count will be exact and will not include deleted or implicitly created records.

  Returned if DB_FAST_STAT is set.

- `u_int32_t bt_over_pg;`

  Number of database overflow pages.

- `uintmax_t bt_over_pgfree;`

  Number of bytes free in database overflow pages.

- `u_int32_t bt_pagecnt;`

  The number of pages in the database. Returned if DB_FAST_STAT is set.

- `u_int32_t bt_pagesize;`

  The underlying database page size, in bytes. Returned if DB_FAST_STAT is set.

- `u_int32_t bt_re_len;`

  The length of fixed-length records. Returned if DB_FAST_STAT is set.

- `u_int32_t bt_re_pad;`

  The padding byte value for fixed-length records. Returned if DB_FAST_STAT is set.

- `u_int32_t bt_version;`

  The version of the Btree database. Returned if DB_FAST_STAT is set.

#### Queue Statistics

In the case of a Queue database, the statistics are stored in a structure of type `DB_QUEUE_STAT`. The following fields will be filled in:

- `u_int32_t qs_cur_recno;`

  Next available record number. Returned if DB_FAST_STAT is set.

- `u_int32_t qs_extentsize;`

  Underlying database extent size, in pages. Returned if DB_FAST_STAT is set.

- `u_int32_t qs_first_recno;`

  First undeleted record in the database. Returned if DB_FAST_STAT is set.

- `u_int32_t qs_magic;`

  Magic number that identifies the file as a Queue file. Returned if DB_FAST_STAT is set.

- `u_int32_t qs_metaflags;`

  Reports internal flags. For internal use only.

- `u_int32_t qs_nkeys;`

  The number of records in the database. If DB_FAST_STAT was specified the count will be the last saved value unless it has never been calculated, in which case it will be 0. Returned if DB_FAST_STAT is set.

- `u_int32_t qs_ndata;`

  The number of records in the database. If DB_FAST_STAT was specified the count will be the last saved value unless it has never been calculated, in which case it will be 0. Returned if DB_FAST_STAT is set.

- `u_int32_t qs_pages;`

  Number of pages in the database.

- `u_int32_t qs_pagesize;`

  Underlying database page size, in bytes. Returned if DB_FAST_STAT is set.

- `u_int32_t qs_pgfree;`

  Number of bytes free in database pages.

- `u_int32_t qs_re_len;`

  The length of the records. Returned if DB_FAST_STAT is set.

- `u_int32_t qs_re_pad;`

  The padding byte value for the records. Returned if DB_FAST_STAT is set.

- `u_int32_t qs_version;`

  The version of the Queue file type. Returned if DB_FAST_STAT is set.

### Errors

The `DB->stat()` method may fail and return one of the following non-zero errors:

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
