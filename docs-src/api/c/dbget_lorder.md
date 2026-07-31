---
title: "DB->get_lorder()"
api-name: "DB->get_lorder()"
source: docs/api_reference/C/dbget_lorder.html
---
## DB-\>get_lorder()

``` c
#include <db.h>

int
DB->get_lorder(DB *db, int *lorderp);  
```

The `DB->get_lorder()` method returns the database byte order; a byte order of 4,321 indicates a big endian order, and a byte order of 1,234 indicates a little endian order. This value is set using the <a href="dbset_lorder.md" class="xref" title="DB-&gt;set_lorder()">DB-&gt;set_lorder()</a> method.

The `DB->get_lorder()` method may be called at any time during the life of the application.

The `DB->get_lorder()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### lorderp

The `DB->get_lorder()` method returns the database byte order in **lorderp**.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>, <a href="dbset_lorder.md" class="xref" title="DB-&gt;set_lorder()">DB-&gt;set_lorder()</a>
