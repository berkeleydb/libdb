---
title: "DB->get_heapsize()"
api-name: "DB->get_heapsize()"
source: docs/api_reference/C/dbget_heapsize.html
---
## DB-\>get_heapsize()

``` c
#include <db.h>

int
DB->get_heapsize(DB *db, u_int32_t *gbytesp, u_int32_t *bytesp);  
```

Used when the underlying database is configured to use the Heap access method. This method returns the maximum size of the database's heap file. This value may be set using the <a href="dbset_heapsize.md" class="xref" title="DB-&gt;set_heapsize()">DB-&gt;set_heapsize()</a> method.

The `DB->get_heapsize()` method may be called at any time during the life of the application.

The `DB->get_heapsize()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### gbytesp

The **gbytesp** parameter references memory into which is copied the maximum number of gigabytes in the heap.

#### bytesp

The **bytesp** parameter references memory into which is copied the additional bytes in the heap.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>, <a href="dbset_heapsize.md" class="xref" title="DB-&gt;set_heapsize()">DB-&gt;set_heapsize()</a>
