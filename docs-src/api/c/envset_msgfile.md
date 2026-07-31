---
title: "DB_ENV->set_msgfile()"
api-name: "DB_ENV->set_msgfile()"
source: docs/api_reference/C/envset_msgfile.html
---
## DB_ENV-\>set_msgfile()

``` c
#include <db.h>

void
DB_ENV->set_msgfile(DB_ENV *dbenv, FILE *msgfile);  
```

There are interfaces in the Berkeley DB library which either directly output informational messages or statistical information, or configure the library to output such messages when performing other operations, for example, <a href="envset_verbose.md" class="xref" title="DB_ENV-&gt;set_verbose()">DB_ENV-&gt;set_verbose()</a> and <a href="envstat.md" class="xref" title="DB_ENV-&gt;stat_print()">DB_ENV-&gt;stat_print()</a>.

The `DB_ENV->set_msgfile()` and <a href="dbset_msgfile.md" class="xref" title="DB-&gt;set_msgfile()">DB-&gt;set_msgfile()</a> methods are used to display these messages for the application. In this case the message will include a trailing \<newline\> character.

Setting **msgfile** to NULL unconfigures the interface.

Alternatively, you can use the <a href="envset_msgcall.md" class="xref" title="DB_ENV-&gt;set_msgcall()">DB_ENV-&gt;set_msgcall()</a> or <a href="dbset_msgcall.md" class="xref" title="DB-&gt;set_msgcall()">DB-&gt;set_msgcall()</a> methods to capture the additional error information in a way that does not use C library FILE \*'s.

The `DB_ENV->set_msgfile()` method configures operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle, not all operations performed on the underlying database environment.

The `DB_ENV->set_msgfile()` method may be called at any time during the life of the application.

### Parameters

#### msgfile

The **msgfile** parameter is a C library FILE \* to be used for displaying messages.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
