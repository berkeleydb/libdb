---
title: "DB->get_msgfile()"
api-name: "DB->get_msgfile()"
source: docs/api_reference/C/dbget_msgfile.html
---
## DB-\>get_msgfile()

``` c
#include <db.h>

void
DB->get_msgfile(DB *db, FILE **msgfilep);  
```

The `DB->get_msgfile()` method returns the `FILE *` used to output informational or statistical messages. This file handle is configured using the <a href="dbset_msgfile.md" class="xref" title="DB-&gt;set_msgfile()">DB-&gt;set_msgfile()</a> method.

The `DB->get_msgfile()` method may be called at any time during the life of the application.

### Parameters

#### msgfilep

The `DB->get_msgfile()` method returns the `FILE *` in **msgfilep**.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>, <a href="dbset_msgfile.md" class="xref" title="DB-&gt;set_msgfile()">DB-&gt;set_msgfile()</a>
