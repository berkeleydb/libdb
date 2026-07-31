---
title: "DB->set_msgfile()"
api-name: "DB->set_msgfile()"
source: docs/api_reference/C/dbset_msgfile.html
---
## DB-\>set_msgfile()

``` c
#include <db.h>

void
DB->set_msgfile(DB *db, FILE *msgfile);  
```

There are interfaces in the Berkeley DB library which either directly output informational messages or statistical information, or configure the library to output such messages when performing other operations, for example, <a href="envset_verbose.md" class="xref" title="DB_ENV-&gt;set_verbose()">DB_ENV-&gt;set_verbose()</a> and <a href="envstat.md" class="xref" title="DB_ENV-&gt;stat_print()">DB_ENV-&gt;stat_print()</a>.

The <a href="envset_msgfile.md" class="xref" title="DB_ENV-&gt;set_msgfile()">DB_ENV-&gt;set_msgfile()</a> and `DB->set_msgfile()` methods are used to display these messages for the application. In this case the message will include a trailing \<newline\> character.

Setting **msgfile** to NULL unconfigures the interface.

Alternatively, you can use the <a href="envset_msgcall.md" class="xref" title="DB_ENV-&gt;set_msgcall()">DB_ENV-&gt;set_msgcall()</a> or <a href="dbset_msgcall.md" class="xref" title="DB-&gt;set_msgcall()">DB-&gt;set_msgcall()</a> methods to capture the additional error information in a way that does not use C library FILE \*'s.

For <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handles opened inside of Berkeley DB environments, calling the `DB->set_msgfile()` method affects the entire environment and is equivalent to calling the <a href="envset_msgfile.md" class="xref" title="DB_ENV-&gt;set_msgfile()">DB_ENV-&gt;set_msgfile()</a> method.

The `DB->set_msgfile()` method configures operations performed using the specified <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle, not all operations performed on the underlying database.

The `DB->set_msgfile()` method may be called at any time during the life of the application.

### Parameters

#### msgfile

The **msgfile** parameter is a C library FILE \* to be used for displaying messages.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
