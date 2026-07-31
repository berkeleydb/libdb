---
title: "DB->get_errpfx()"
api-name: "DB->get_errpfx()"
source: docs/api_reference/C/dbget_errpfx.html
---
## DB-\>get_errpfx()

``` c
#include <db.h>

void DB->get_errpfx(DB *db, const char **errpfxp);  
```

The `DB->get_errpfx()` method returns the error prefix.

The `DB->get_errpfx()` method may be called at any time during the life of the application.

### Parameters

#### errpfxp

The `DB->get_errpfx()` method returns a reference to the error prefix in **errpfxp**.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>, <a href="dbset_errpfx.md" class="xref" title="DB-&gt;set_errpfx()">DB-&gt;set_errpfx()</a>
