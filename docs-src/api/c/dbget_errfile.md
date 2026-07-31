---
title: "DB->get_errfile()"
api-name: "DB->get_errfile()"
source: docs/api_reference/C/dbget_errfile.html
---
## DB-\>get_errfile()

``` c
#include <db.h>

void
DB->get_errfile(DB *db, FILE **errfilep);  
```

The `DB->get_errfile()` method returns the `FILE *`, as set by the <a href="dbset_errfile.md" class="xref" title="DB-&gt;set_errfile()">DB-&gt;set_errfile()</a> method.

The `DB->get_errfile()` method may be called at any time during the life of the application.

### Parameters

#### errfilep

The `DB->get_errfile()` method returns the FILE \* in **errfilep**.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>, <a href="dbset_errfile.md" class="xref" title="DB-&gt;set_errfile()">DB-&gt;set_errfile()</a>
