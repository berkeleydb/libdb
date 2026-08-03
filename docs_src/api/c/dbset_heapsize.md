---
title: "DB->set_heapsize()"
api-name: "DB->set_heapsize()"
source: docs/api_reference/C/dbset_heapsize.html
---
## DB-\>set_heapsize()

``` c
#include <db.h>

int
DB->set_heapsize(DB *db,
    u_int32_t gbytes, u_int32_t bytes, u_int32_t flags);  
```

Sets the maximum on-disk database file size used by a database configured to use the Heap access method. If this method is never called, the database's file size can grow without bound. If this method is called, then the heap file can never grow larger than the limit defined by this method. In that case, attempts to update or create records in a Heap database that has reached its maximum size will result in a `DB_HEAP_FULL` error return.

The size specified to this method must be at least three times the database page size. That is, a Heap database must contain at least three database pages. You can set the database page size using the <a href="dbset_pagesize.md" class="xref" title="DB-&gt;set_pagesize()">DB-&gt;set_pagesize()</a> method.

The `DB->set_heapsize()` method may not be called after the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called. Further, if this method is called on an existing Heap database, the size specified here must match the size used to create the database. Note, however, that specifying an incorrect size to this method will not result in an error return (`EINVAL`) until the database is opened.

The `DB->set_heapsize()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### gbytes

The size of the heap is set to **gbytes** gigabytes plus **bytes**.

#### bytes

The size of the heap is set to **gbytes** gigabytes plus **bytes**.

#### flags

The **flags** parameter is currently unused, and must be set to 0.

### Errors

The `DB->set_heapsize()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the specified heap size was too small; the method was called after <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
