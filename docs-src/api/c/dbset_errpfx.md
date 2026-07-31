---
title: "DB->set_errpfx()"
api-name: "DB->set_errpfx()"
source: docs/api_reference/C/dbset_errpfx.html
---
## DB-\>set_errpfx()

``` c
#include <db.h>

void
DB->set_errpfx(DB *db, const char *errpfx);  
```

Set the prefix string that appears before error messages issued by Berkeley DB.

The `DB->set_errpfx()` and <a href="envset_errpfx.md" class="xref" title="DB_ENV-&gt;set_errpfx()">DB_ENV-&gt;set_errpfx()</a> methods do not copy the memory to which the **errpfx** parameter refers; rather, they maintain a reference to it. Although this allows applications to modify the error message prefix at any time (without repeatedly calling the interfaces), it means the memory must be maintained until the handle is closed.

For <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handles opened inside of Berkeley DB environments, calling the `DB->set_errpfx()` method affects the entire environment and is equivalent to calling the <a href="envset_errpfx.md" class="xref" title="DB_ENV-&gt;set_errpfx()">DB_ENV-&gt;set_errpfx()</a> method.

The `DB->set_errpfx()` method configures operations performed using the specified <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle, not all operations performed on the underlying database.

The `DB->set_errpfx()` method may be called at any time during the life of the application.

### Parameters

#### errpfx

The **errpfx** parameter is the application-specified error prefix for additional error messages.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
