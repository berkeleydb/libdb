---
title: "DB->set_msgcall()"
api-name: "DB->set_msgcall()"
source: docs/api_reference/C/dbset_msgcall.html
---
## DB-\>set_msgcall()

``` c
#include <db.h>

void
DB->set_msgcall(DB *,
    void (*db_msgcall_fcn)(const DB_ENV *dbenv, char *msg));  
```

There are interfaces in the Berkeley DB library which either directly output informational messages or statistical information, or configure the library to output such messages when performing other operations, for example, <a href="envset_verbose.md" class="xref" title="DB_ENV-&gt;set_verbose()">DB_ENV-&gt;set_verbose()</a> and <a href="envstat.md" class="xref" title="DB_ENV-&gt;stat_print()">DB_ENV-&gt;stat_print()</a>.

The <a href="envset_msgcall.md" class="xref" title="DB_ENV-&gt;set_msgcall()">DB_ENV-&gt;set_msgcall()</a> and `DB->set_msgcall()` methods are used to pass these messages to the application, and Berkeley DB will call **db_msgcall_fcn** with each message. It is up to the **db_msgcall_fcn** function to display the message in an appropriate manner.

Setting **db_msgcall_fcn** to NULL unconfigures the callback interface.

Alternatively, you can use the <a href="dbset_msgfile.md" class="xref" title="DB-&gt;set_msgfile()">DB-&gt;set_msgfile()</a> or <a href="envset_msgfile.md" class="xref" title="DB_ENV-&gt;set_msgfile()">DB-&gt;set_msgfile()</a> methods to display the messages via a C library FILE \*.

For <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handles opened inside of Berkeley DB environments, calling the `DB->set_msgcall()` method affects the entire environment and is equivalent to calling the `DB_ENV->set_msgcall()` method.

The `DB->set_msgcall()` method configures operations performed using the specified <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle, not all operations performed on the underlying database.

The `DB->set_msgcall()` method may be called at any time during the life of the application.

### Note

Berkeley DB is not re-entrant. Callback functions should not attempt to make library calls (for example, to release locks or close open handles). Re-entering Berkeley DB is not guaranteed to work correctly, and the results are undefined.

### Parameters

#### db_msgcall_fcn

The **db_msgcall_fcn** parameter is the application-specified message reporting function. The function takes two parameters:

- `dbenv`

  The **dbenv** parameter is the enclosing database environment.

- `msg`

  The **msg** parameter is the message string.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
