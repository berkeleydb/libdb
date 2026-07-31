---
title: "DB_ENV->get_errfile()"
api-name: "DB_ENV->get_errfile()"
source: docs/api_reference/C/envget_errfile.html
---
## DB_ENV-\>get_errfile()

``` c
#include <db.h>

void
DB_ENV->get_errfile(DB_ENV *dbenv, FILE **errfilep);  
```

The `DB_ENV->get_errfile()` method returns the FILE \* used for displaying additional Berkeley DB error messages. This C library is set using the <a href="envset_errfile.md" class="xref" title="DB_ENV-&gt;set_errfile()">DB_ENV-&gt;set_errfile()</a> method.

The `DB_ENV->get_errfile()` method may be called at any time during the life of the application.

### Parameters

#### errfilep

The `DB_ENV->get_errfile()` method returns the FILE \* in **errfilep**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
