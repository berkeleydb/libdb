---
title: "DB_ENV->set_errpfx()"
api-name: "DB_ENV->set_errpfx()"
source: docs/api_reference/C/envset_errpfx.html
---
## DB_ENV-\>set_errpfx()

``` c
#include <db.h>

void
DB_ENV->set_errpfx(DB_ENV *dbenv, const char *errpfx);  
```

Set the prefix string that appears before error messages issued by Berkeley DB.

The <a href="dbset_errpfx.md" class="xref" title="DB-&gt;set_errpfx()">DB-&gt;set_errpfx()</a> and `DB_ENV->set_errpfx()` methods do not copy the memory to which the **errpfx** parameter refers; rather, they maintain a reference to it. Although this allows applications to modify the error message prefix at any time (without repeatedly calling the interfaces), it means the memory must be maintained until the handle is closed.

The `DB_ENV->set_errpfx()` method configures operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle, not all operations performed on the underlying database environment.

The `DB_ENV->set_errpfx()` method may be called at any time during the life of the application.

### Parameters

#### errpfx

The **errpfx** parameter is the application-specified error prefix for additional error messages.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
