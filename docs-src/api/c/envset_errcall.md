---
title: "DB_ENV->set_errcall()"
api-name: "DB_ENV->set_errcall()"
source: docs/api_reference/C/envset_errcall.html
---
## DB_ENV-\>set_errcall()

``` c
#include <db.h>

void
DB_ENV->set_errcall(DB_ENV *dbenv, void (*db_errcall_fcn)
    (const DB_ENV *dbenv, const char *errpfx, const char *msg));  
```

When an error occurs in the Berkeley DB library, a Berkeley DB error or an error return value is returned by the interface. In some cases, however, the **errno** value may be insufficient to completely describe the cause of the error, especially during initial application debugging.

The `DB_ENV->set_errcall()` and <a href="envset_errcall.md" class="xref" title="DB_ENV-&gt;set_errcall()">DB_ENV-&gt;set_errcall()</a> methods are used to enhance the mechanism for reporting error messages to the application. In some cases, when an error occurs, Berkeley DB will call **db_errcall_fcn** with additional error information. It is up to the **db_errcall_fcn** function to display the error message in an appropriate manner.

Setting **db_errcall_fcn** to NULL unconfigures the callback interface.

Alternatively, you can use the <a href="dbset_errfile.md" class="xref" title="DB-&gt;set_errfile()">DB-&gt;set_errfile()</a> or <a href="envset_errfile.md" class="xref" title="DB_ENV-&gt;set_errfile()">DB-&gt;set_errfile()</a> methods to display the additional information via a C library `FILE *`.

This error-logging enhancement does not slow performance or significantly increase application size, and may be run during normal operation as well as during application debugging.

The `DB_ENV->set_errcall()` method configures operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle, not all operations performed on the underlying database environment.

The `DB_ENV->set_errcall()` method may be called at any time during the life of the application.

### Note

Berkeley DB is not re-entrant. Callback functions should not attempt to make library calls (for example, to release locks or close open handles). Re-entering Berkeley DB is not guaranteed to work correctly, and the results are undefined.

### Parameters

#### db_errcall_fcn

The **db_errcall_fcn** parameter is the application-specified error reporting function. The function takes three parameters:

- `dbenv`

  The **dbenv** parameter is the enclosing database environment.

- `errpfx`

  The **errpfx** parameter is the prefix string (as previously set by <a href="dbset_errpfx.md" class="xref" title="DB-&gt;set_errpfx()">DB-&gt;set_errpfx()</a> or <a href="envset_errpfx.md" class="xref" title="DB_ENV-&gt;set_errpfx()">DB_ENV-&gt;set_errpfx()</a>).

- `msg`

  The **msg** parameter is the error message string.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
