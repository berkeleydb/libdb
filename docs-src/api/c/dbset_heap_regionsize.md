---
title: "DB->set_heap_regionsize()"
api-name: "DB->set_heap_regionsize()"
source: docs/api_reference/C/dbset_heap_regionsize.html
---
## DB-\>set_heap_regionsize()

``` c
#include <db.h>

int
DB->set_heap_regionsize(DB *db, u_int32_t npages);  
```

Sets the number of pages in a region of a database configured to use the Heap access method. If this method is never called, the default region size for the database's page size will be used. You can set the database page size using the <a href="dbset_pagesize.md" class="xref" title="DB-&gt;set_pagesize()">DB-&gt;set_pagesize()</a> method.

The `DB->set_heap_regionsize()` method may not be called after the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called. If the database already exists when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called, the information specified to `DB->set_heap_regionsize()` will be ignored. If the specified region size is larger than the maximum region size for the database's page size, an error will be returned when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called. The maximum allowable region size will be included in the error message.

The `DB->set_heap_regionsize()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### npages

The **npages** parameter is the number of pages in a Heap database region.

### Errors

The `DB->set_heap_regionsize()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the specified region size was too small; the method was called after <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>, <a href="dbget_heap_regionsize.md" class="xref" title="DB-&gt;get_heap_regionsize()">DB-&gt;get_heap_regionsize()</a>
