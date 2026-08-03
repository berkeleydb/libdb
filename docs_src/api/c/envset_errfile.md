---
title: "DB_ENV->set_errfile()"
api-name: "DB_ENV->set_errfile()"
source: docs/api_reference/C/envset_errfile.html
---
## DB_ENV-\>set_errfile()

``` c
#include <db.h>

void
DB_ENV->set_errfile(DB_ENV *dbenv, FILE *errfile);  
```

When an error occurs in the Berkeley DB library, a Berkeley DB error or an error return value is returned by the interface. In some cases, however, the return value may be insufficient to completely describe the cause of the error especially during initial application debugging.

The `DB_ENV->set_errfile()` and <a href="dbset_errfile.md" class="xref" title="DB-&gt;set_errfile()">DB-&gt;set_errfile()</a> methods are used to enhance the mechanism for reporting error messages to the application by setting a C library FILE \* to be used for displaying additional Berkeley DB error messages. In some cases, when an error occurs, Berkeley DB will output an additional error message to the specified file reference.

Alternatively, you can use the <a href="envset_errcall.md" class="xref" title="DB_ENV-&gt;set_errcall()">DB_ENV-&gt;set_errcall()</a> or <a href="dbset_errcall.md" class="xref" title="DB-&gt;set_errcall()">DB-&gt;set_errcall()</a> methods to capture the additional error information in a way that does not use C library FILE \*'s.

The error message will consist of the prefix string and a colon ("**:**") (if a prefix string was previously specified using <a href="dbset_errpfx.md" class="xref" title="DB-&gt;set_errpfx()">DB-&gt;set_errpfx()</a> or <a href="envset_errpfx.md" class="xref" title="DB_ENV-&gt;set_errpfx()">DB_ENV-&gt;set_errpfx()</a> ), an error string, and a trailing \<newline\> character.

The default configuration when applications first create <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> or <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handles is as if the <a href="dbset_errfile.md" class="xref" title="DB-&gt;set_errfile()">DB-&gt;set_errfile()</a> or `DB_ENV->set_errfile()` methods were called with the standard error output (stderr) specified as the FILE \* argument. Applications wanting no output at all can turn off this default configuration by calling the <a href="dbset_errfile.md" class="xref" title="DB-&gt;set_errfile()">DB-&gt;set_errfile()</a> or `DB_ENV->set_errfile()` methods with NULL as the FILE \* argument. Additionally, explicitly configuring the error output channel using any of the following methods will also turn off this default output for the application:

- `DB_ENV->set_errfile()`

- <a href="dbset_errfile.md" class="xref" title="DB-&gt;set_errfile()">DB-&gt;set_errfile()</a>

- <a href="envset_errcall.md" class="xref" title="DB_ENV-&gt;set_errcall()">DB_ENV-&gt;set_errcall()</a>

- <a href="dbset_errcall.md" class="xref" title="DB-&gt;set_errcall()">DB-&gt;set_errcall()</a>

This error logging enhancement does not slow performance or significantly increase application size, and may be run during normal operation as well as during application debugging.

The `DB_ENV->set_errfile()` method configures operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle, not all operations performed on the underlying database environment.

The `DB_ENV->set_errfile()` method may be called at any time during the life of the application.

### Parameters

#### errfile

The **errfile** parameter is a C library FILE \* to be used for displaying additional Berkeley DB error information.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
