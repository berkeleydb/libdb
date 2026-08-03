---
title: "DB->get_byteswapped()"
api-name: "DB->get_byteswapped()"
source: docs/api_reference/C/dbget_byteswapped.html
---
## DB-\>get_byteswapped()

``` c
#include <db.h>

int
DB->get_byteswapped(DB *db, int *isswapped);  
```

The `DB->get_byteswapped()` method returns whether the underlying database files were created on an architecture of the same byte order as the current one, or if they were not (that is, big-endian on a little-endian machine, or vice versa). This information may be used to determine whether application data needs to be adjusted for this architecture or not.

The `DB->get_byteswapped()` method may not be called before the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called.

The `DB->get_byteswapped()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### isswapped

If the underlying database files were created on an architecture of the same byte order as the current one, 0 is stored into the memory location referenced by **isswapped**. If the underlying database files were created on an architecture of a different byte order as the current one, 1 is stored into the memory location referenced by **isswapped**.

### Errors

The `DB->get_byteswapped()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called before <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
