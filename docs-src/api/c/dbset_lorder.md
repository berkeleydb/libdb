---
title: "DB->set_lorder()"
api-name: "DB->set_lorder()"
source: docs/api_reference/C/dbset_lorder.html
---
## DB-\>set_lorder()

``` c
#include <db.h>

int
DB->set_lorder(DB *db, int lorder);  
```

Set the byte order for integers in the stored database metadata. The host byte order of the machine where the Berkeley DB library was compiled will be used if no byte order is set.

**The access methods provide no guarantees about the byte ordering of the application data stored in the database, and applications are responsible for maintaining any necessary ordering.**

The `DB->set_lorder()` method configures a database, not only operations performed using the specified <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle.

The `DB->set_lorder()` method may not be called after the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called. If the database already exists when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called, the information specified to `DB->set_lorder()` will be ignored.

If creating additional databases in a single physical file, information specified to `DB->set_lorder()` will be ignored and the byte order of the existing databases will be used.

The `DB->set_lorder()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### lorder

The **lorder** parameter should represent the byte order as an integer; for example, big endian order is the number 4,321, and little endian order is the number 1,234.

### Errors

The `DB->set_lorder()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
