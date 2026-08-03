---
title: "DB_ENV->get_errpfx()"
api-name: "DB_ENV->get_errpfx()"
source: docs/api_reference/C/envget_errpfx.html
---
## DB_ENV-\>get_errpfx()

``` c
#include <db.h>

void
DB_ENV->get_errpfx(DB_ENV *dbenv, const char **errpfxp);  
```

The `DB_ENV->get_errpfx()` method returns the error prefix that appears before error messages issued by Berkeley DB. This error prefix is set using the <a href="envset_errpfx.md" class="xref" title="DB_ENV-&gt;set_errpfx()">DB_ENV-&gt;set_errpfx()</a> method.

The `DB_ENV->get_errpfx()` method may be called at any time during the life of the application.

### Parameters

#### errpfxp

The `DB_ENV->get_errpfx()` method returns a reference to the error prefix in **errpfxp**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
