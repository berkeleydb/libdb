---
title: "DB->get_heap_regionsize()"
api-name: "DB->get_heap_regionsize()"
source: docs/api_reference/C/dbget_heap_regionsize.html
---
## DB-\>get_heap_regionsize()

``` c
#include <db.h>

int
DB->get_heap_regionsize(DB *db, u_int32_t *npagesp);  
```

Used when the underlying database is configured to use the Heap access method. This method returns the number of pages in a region. This value may be set using the <a href="dbset_heap_regionsize.md" class="xref" title="DB-&gt;set_heap_regionsize()">DB-&gt;set_heap_regionsize()</a> method.

The `DB->get_heap_regionsize()` method may be called at any time during the life of the application.

The `DB->get_heap_regionsize()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### npagesp

The **npagesp** parameter references memory into which is copied the number of pages in a region.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>, <a href="dbset_heap_regionsize.md" class="xref" title="DB-&gt;set_heap_regionsize()">DB-&gt;set_heap_regionsize()</a>
