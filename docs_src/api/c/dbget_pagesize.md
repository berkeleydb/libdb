---
title: "DB->get_pagesize()"
api-name: "DB->get_pagesize()"
source: docs/api_reference/C/dbget_pagesize.html
---
## DB-\>get_pagesize()

``` c
#include <db.h>

int
DB->get_pagesize(DB *db, u_int32_t *pagesizep);  
```

The `DB->get_pagesize()` method returns the database's current page size, as set by the <a href="dbset_pagesize.md" class="xref" title="DB-&gt;set_pagesize()">DB-&gt;set_pagesize()</a> method. Note that if `DB->set_pagesize()` was not called by your application, then the default pagesize is selected based on the underlying filesystem I/O block size. If you call `DB->get_pagesize()` before you have opened the database, the value returned by this method is therefore the underlying filesystem I/O block size.

The `DB->get_pagesize()` method may be called only after the database has been opened.

The `DB->get_pagesize()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### pagesizep

The `DB->get_pagesize()` method returns the page size in **pagesizep**.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>, <a href="dbset_pagesize.md" class="xref" title="DB-&gt;set_pagesize()">DB-&gt;set_pagesize()</a>
