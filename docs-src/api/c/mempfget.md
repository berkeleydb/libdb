---
title: "DB_MPOOLFILE->get()"
api-name: "DB_MPOOLFILE->get()"
source: docs/api_reference/C/mempfget.html
---
## DB_MPOOLFILE-\>get()

``` c
#include <db.h>

int
DB_MPOOLFILE->get(DB_MPOOLFILE *mpf,
    db_pgno_t *pgnoaddr, DB_TXN * txnid, u_int32_t flags, void **pagep);  
```

The `DB_MPOOLFILE->get()` method returns pages from the cache.

All pages returned by `DB_MPOOLFILE->get()` will be retained (that is, <span class="emphasis">*latched*</span>) in the cache until a subsequent call to <a href="mempput.md" class="xref" title="DB_MPOOLFILE-&gt;put()">DB_MPOOLFILE-&gt;put()</a>. There is no deadlock detection among latches so care must be taken in the application if the DB_MPOOL_DIRTY or DB_MPOOL_EDIT flags are used as these get exlusive latches on the pages.

The returned page is **size_t** type aligned.

Fully or partially created pages have all their bytes set to a nul byte, unless the <a href="mempset_clear_len.md" class="xref" title="DB_MPOOLFILE-&gt;set_clear_len()">DB_MPOOLFILE-&gt;set_clear_len()</a> method was called to specify other behavior before the file was opened.

The `DB_MPOOLFILE->get()` method will return `DB_PAGE_NOTFOUND` if the requested page does not exist and DB_MPOOL_CREATE was not set. Unless otherwise specified, the `DB_MPOOLFILE->get()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flags

The **flags** parameter must be set to 0 or by bitwise inclusively **OR**'ing together one or more of the following values:

- `DB_MPOOL_CREATE`

  If the specified page does not exist, create it. In this case, the <a href="mempregister.md" class="link" title="DB_ENV-&gt;memp_register()">pgin</a> method, if specified, is called.

- `DB_MPOOL_DIRTY`

  The page will be modified and must be written to the source file before being evicted from the cache. For files open with the <a href="dbopen.md#dbopen_DB_MULTIVERSION" class="link">DB_MULTIVERSION</a> flag set, a new copy of the page will be made if this is the first time the specified transaction is modifying it. A page fetched with the `DB_MPOOL_DIRTY` flag will be **exclusively latched** until a subsequent call to <a href="mempput.md" class="xref" title="DB_MPOOLFILE-&gt;put()">DB_MPOOLFILE-&gt;put()</a>.

- `DB_MPOOL_EDIT`

  The page will be modified and must be written to the source file before being evicted from the cache. No copy of the page will be made, regardless of the <a href="dbopen.md#dbopen_DB_MULTIVERSION" class="link">DB_MULTIVERSION</a> setting. This flag is only intended for use in situations where a transaction handle is not available, such as during aborts or recovery. A page fetched with the `DB_MPOOL_EDIT` flag will be **exclusively latched** until a subsequent call to <a href="mempput.md" class="xref" title="DB_MPOOLFILE-&gt;put()">DB_MPOOLFILE-&gt;put()</a>.

- `DB_MPOOL_LAST`

  Return the last page of the source file, and copy its page number into the memory location to which **pgnoaddr** refers.

- `DB_MPOOL_NEW`

  Create a new page in the file, and copy its page number into the memory location to which **pgnoaddr** refers. In this case, the `pgin_fcn` callback, if specified on <a href="mempregister.md" class="xref" title="DB_ENV-&gt;memp_register()">DB_ENV-&gt;memp_register()</a>, is **not** called.

The `DB_MPOOL_CREATE`, `DB_MPOOL_LAST`, and `DB_MPOOL_NEW` flags are mutually exclusive.

#### pagep

The **pagep** parameter references memory into which a pointer to the returned page is copied.

#### pgnoaddr

If the **flags** parameter is set to `DB_MPOOL_LAST` or `DB_MPOOL_NEW`, the page number of the created page is copied into the memory location to which the **pgnoaddr** parameter refers. Otherwise, the **pgnoaddr** parameter is the page to create or retrieve.

### Note

Page numbers begin at 0; that is, the first page in the file is page number 0, not page number 1.

#### txnid

If the operation is part of an application-specified transaction, the **txnid** parameter is a transaction handle returned from <a href="txnbegin.md" class="xref" title="DB_ENV-&gt;txn_begin()">DB_ENV-&gt;txn_begin()</a>; otherwise NULL. A transaction is required if the file is open for multiversion concurrency control by passing <a href="dbopen.md#dbopen_DB_MULTIVERSION" class="link">DB_MULTIVERSION</a> to <a href="mempfopen.md" class="xref" title="DB_MPOOLFILE-&gt;open()">DB_MPOOLFILE-&gt;open()</a> and the DB_MPOOL_DIRTY, DB_MPOOL_CREATE or DB_MPOOL_NEW flags were specified. Otherwise it is ignored.

### Errors

The `DB_MPOOLFILE->get()` method may fail and return one of the following non-zero errors:

#### EACCES

The `DB_MPOOL_DIRTY` or `DB_MPOOL_EDIT` flag was set and the source file was not opened for writing.

#### EAGAIN

The page reference count has overflowed. (This should never happen unless there is a bug in the application.)

#### EINVAL

If the `DB_MPOOL_NEW` flag was set, and the source file was not opened for writing; more than one of `DB_MPOOL_CREATE`, `DB_MPOOL_LAST`, and `DB_MPOOL_NEW` was set; or if an invalid flag value or parameter was specified.

#### DB_LOCK_DEADLOCK

For transactions configured with <a href="txnbegin.md#txnbegin_DB_TXN_SNAPSHOT" class="link">DB_TXN_SNAPSHOT</a>, the page has been modified since the transaction began.

#### ENOMEM

The cache is full, and no more pages will fit in the cache.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>
