---
title: "DB_ENV->get_msgfile()"
api-name: "DB_ENV->get_msgfile()"
source: docs/api_reference/C/envget_msgfile.html
---
## DB_ENV-\>get_msgfile()

``` c
#include <db.h>

void
DB_ENV->get_msgfile(DB_ENV *dbenv, FILE **msgfilep);  
```

The `DB_ENV->get_msgfile()` method returns the `FILE *` used for displaying messages. This is set using the <a href="envset_msgfile.md" class="xref" title="DB_ENV-&gt;set_msgfile()">DB_ENV-&gt;set_msgfile()</a> method.

The `DB_ENV->get_msgfile()` method may be called at any time during the life of the application.

### Parameters

#### msgfilep

The `DB_ENV->get_msgfile()` method returns the FILE \* in **msgfilep**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>, <a href="envset_msgfile.md" class="xref" title="DB_ENV-&gt;set_msgfile()">DB_ENV-&gt;set_msgfile()</a>
