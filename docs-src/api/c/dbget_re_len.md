---
title: "DB->get_re_len()"
api-name: "DB->get_re_len()"
source: docs/api_reference/C/dbget_re_len.html
---
## DB-\>get_re_len()

``` c
#include <db.h>

int
DB->get_re_len(DB *db, u_int32_t *re_lenp);  
```

The `DB->get_re_len()` method returns the length of the records held in a Queue access method database. This value can be set using the <a href="dbset_re_len.md" class="xref" title="DB-&gt;set_re_len()">DB-&gt;set_re_len()</a> method.

The `DB->get_re_len()` method may be called only after the database has been opened.

The `DB->get_re_len()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### re_lenp

The `DB->get_re_len()` method returns the record length in **re_lenp**. If the record length has never been set using <a href="dbset_re_len.md" class="xref" title="DB-&gt;set_re_len()">DB-&gt;set_re_len()</a>, then `0` is returned.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>, <a href="dbset_re_len.md" class="xref" title="DB-&gt;set_re_len()">DB-&gt;set_re_len()</a>
