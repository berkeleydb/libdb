---
title: "DB->set_pagesize()"
api-name: "DB->set_pagesize()"
source: docs/api_reference/C/dbset_pagesize.html
---
## DB-\>set_pagesize()

``` c
#include <db.h>

int
DB->set_pagesize(DB *db, u_int32_t pagesize);  
```

Set the size of the pages used to hold items in the database, in bytes. The minimum page size is 512 bytes, the maximum page size is 64K bytes, and the page size must be a power-of-two. If the page size is not explicitly set, one is selected based on the underlying filesystem I/O block size. The automatically selected size has a lower limit of 512 bytes and an upper limit of 16K bytes.

For information on tuning the Berkeley DB page size, see <a href="../../guides/programmer_reference/general_am_conf.md#am_conf_pagesize" class="olink">Selecting a page size</a>.

The `DB->set_pagesize()` method configures a database, not only operations performed using the specified <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle.

The `DB->set_pagesize()` method may not be called after the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called. If the database already exists when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called, the information specified to `DB->set_pagesize()` will be ignored.

If creating additional databases in a single physical file, information specified to `DB->set_pagesize()` will be ignored and the page size of the existing databases will be used.

The `DB->set_pagesize()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### pagesize

The **pagesize** parameter sets the database page size.

### Errors

The `DB->set_pagesize()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
