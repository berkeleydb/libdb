---
title: "DB->get_re_delim()"
api-name: "DB->get_re_delim()"
source: docs/api_reference/C/dbget_re_delim.html
---
## DB-\>get_re_delim()

``` c
#include <db.h>

int
DB->get_re_delim(DB *db, int *delimp);  
```

The `DB->get_re_delim()` method returns the delimiting byte, which is used to mark the end of a record in the backing source file for the Recno access method. This value is set using the <a href="dbset_re_delim.md" class="xref" title="DB-&gt;set_re_delim()">DB-&gt;set_re_delim()</a> method.

The `DB->get_re_delim()` method may be called only after the database has been opened.

The `DB->get_re_delim()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### delimp

The `DB->get_re_delim()` method returns the delimiting byte in **delimp**. If this method is called on a handle that has not yet been opened, then the default delimiting byte is returned. See <a href="dbset_re_delim.md" class="xref" title="DB-&gt;set_re_delim()">DB-&gt;set_re_delim()</a> for details.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>, <a href="dbset_re_delim.md" class="xref" title="DB-&gt;set_re_delim()">DB-&gt;set_re_delim()</a>
