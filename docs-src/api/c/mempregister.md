---
title: "DB_ENV->memp_register()"
api-name: "DB_ENV->memp_register()"
source: docs/api_reference/C/mempregister.html
---
## DB_ENV-\>memp_register()

``` c
#include <db.h>

int
DB_ENV->memp_register(DB_ENV *env, int ftype,
    int (*pgin_fcn)(DB_ENV *env, db_pgno_t pgno, void *pgaddr, 
    DBT *pgcookie), int (*pgout_fcn)(DB_ENV *env, db_pgno_t pgno, 
    void *pgaddr, DBT *pgcookie));  
```

The `DB_ENV->memp_register()` method registers page-in and page-out functions for files of type **ftype** in the cache.

If the **pgin_fcn** function is non-NULL, it is called each time a page is read into the cache from a file of type **ftype**, or a page is created for a file of type **ftype** (see the DB_MPOOL_CREATE flag for the <a href="mempfget.md" class="xref" title="DB_MPOOLFILE-&gt;get()">DB_MPOOLFILE-&gt;get()</a> method).

If the **pgout_fcn** function is non-NULL, it is called each time a page is written to a file of type **ftype**.

The purpose of the `DB_ENV->memp_register()` function is to support processing when pages are entered into, or flushed from, the cache. For example, this functionality might be used to do byte-endian conversion as pages are read from, or written to, the underlying file.

A file type must be specified to make it possible for unrelated threads or processes that are sharing a cache, to evict each other's pages from the cache. During initialization, applications should call `DB_ENV->memp_register()` for each type of file requiring input or output processing that will be sharing the underlying cache. (No registry is necessary for the standard Berkeley DB access method types because <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> registers them separately.)

If a thread or process does not call `DB_ENV->memp_register()` for a file type, it is impossible for it to evict pages for any file requiring input or output processing from the cache. For this reason, `DB_ENV->memp_register()` should always be called by each application sharing a cache for each type of file included in the cache, regardless of whether or not the application itself uses files of that type.

The `DB_ENV->memp_register()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### ftype

The **ftype** parameter specifies the type of file for which the page-in and page-out functions will be called.

The **ftype** value for a file must be a non-zero positive number less than 128 (0 and negative numbers are reserved for internal use by the Berkeley DB library).

#### pgin_fcn, pgout_fcn

The page-in and page-out functions.

The **pgin_fcn** and **pgout_fcn** functions are called with a reference to the current database environment, the page number being read or written, a pointer to the page being read or written, and any parameter **pgcookie** that was specified to the <a href="mempset_pgcookie.md" class="xref" title="DB_MPOOLFILE-&gt;set_pgcookie()">DB_MPOOLFILE-&gt;set_pgcookie()</a> method.

The **pgin_fcn** and **pgout_fcn** functions should return 0 on success, and a non-zero value on failure, in which case the shared Berkeley DB library function calling it will also fail, returning that non-zero value. The non-zero value should be selected from values outside of the Berkeley DB library namespace.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>
