---
title: "DB->get_flags()"
api-name: "DB->get_flags()"
source: docs/api_reference/C/dbget_flags.html
---
## DB-\>get_flags()

``` c
#include <db.h>

int
DB->get_flags(DB *db, u_int32_t *flagsp);  
```

The `DB->get_flags()` method returns the current database flags as set by the <a href="dbset_flags.md" class="xref" title="DB-&gt;set_flags()">DB-&gt;set_flags()</a> method.

The `DB->get_flags()` method may be called at any time during the life of the application.

The `DB->get_flags()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flagsp

The `DB->get_flags()` method returns the current flags in **flagsp**.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>, <a href="dbset_flags.md" class="xref" title="DB-&gt;set_flags()">DB-&gt;set_flags()</a>
