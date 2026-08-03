---
title: "DB->get_re_source()"
api-name: "DB->get_re_source()"
source: docs/api_reference/C/dbget_re_source.html
---
## DB-\>get_re_source()

``` c
#include <db.h>

int
DB->get_re_source(DB *db, const char **sourcep);  
```

The `DB->get_re_source()` method returns the source file used by the Recno access method. This file is configured for the Recno access method using the <a href="dbset_re_source.md" class="xref" title="DB-&gt;set_re_source()">DB-&gt;set_re_source()</a> method.

The `DB->get_re_source()` method may be called only after the database has been opened.

The `DB->get_re_source()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### sourcep

The `DB->get_re_source()` method returns a reference to the source file in **sourcep**.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>, <a href="dbset_re_source.md" class="xref" title="DB-&gt;set_re_source()">DB-&gt;set_re_source()</a>
