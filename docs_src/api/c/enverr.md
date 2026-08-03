---
title: "DB_ENV->err()"
api-name: "DB_ENV->err()"
source: docs/api_reference/C/enverr.html
---
## DB_ENV-\>err()

``` c
#include <db.h>

void
DB_ENV->err(DB_ENV *dbenv, int error, const char *fmt, ...); 

void
DB_ENV->errx(DB_ENV *dbenv, const char *fmt, ...);  
```

The `DB_ENV->err()`, `DB_ENV->errx,()`, <a href="dberr.md" class="xref" title="DB-&gt;err()">DB-&gt;err()</a> and `DB->errx()` methods provide error-messaging functionality for applications written using the Berkeley DB library.

The <a href="dberr.md" class="xref" title="DB-&gt;err()">DB-&gt;err()</a> and <a href="enverr.md" class="xref" title="DB_ENV-&gt;err()">DB_ENV-&gt;err()</a> methods constructs an error message consisting of the following elements:

- **An optional prefix string**

  If no error callback function has been set using the <a href="envset_errcall.md" class="xref" title="DB_ENV-&gt;set_errcall()">DB_ENV-&gt;set_errcall()</a> method, any prefix string specified using the <a href="envset_errpfx.md" class="xref" title="DB_ENV-&gt;set_errpfx()">DB_ENV-&gt;set_errpfx()</a> method, followed by two separating characters: a colon and a \<space\> character.

- **An optional printf-style message**

  The supplied message **fmt**, if non-NULL, in which the ANSI C X3.159-1989 (ANSI C) printf function specifies how subsequent parameters are converted for output.

- **A separator**

  Two separating characters: a colon and a \<space\> character.

- **A standard error string**

  The standard system or Berkeley DB library error string associated with the **error** value, as returned by the <a href="envstrerror.md" class="xref" title="db_strerror">db_strerror</a> method.

This constructed error message is then handled as follows:

- If an error callback function has been set (see <a href="dbset_errcall.md" class="xref" title="DB-&gt;set_errcall()">DB-&gt;set_errcall()</a> and <a href="envset_errcall.md" class="xref" title="DB_ENV-&gt;set_errcall()">DB_ENV-&gt;set_errcall()</a>), that function is called with two parameters: any prefix string specified (see <a href="dbset_errpfx.md" class="xref" title="DB-&gt;set_errpfx()">DB-&gt;set_errpfx()</a> and <a href="envset_errpfx.md" class="xref" title="DB_ENV-&gt;set_errpfx()">DB_ENV-&gt;set_errpfx()</a>) and the error message.

- If a C library FILE \* has been set (see <a href="dbset_errfile.md" class="xref" title="DB-&gt;set_errfile()">DB-&gt;set_errfile()</a> and <a href="envset_errfile.md" class="xref" title="DB_ENV-&gt;set_errfile()">DB_ENV-&gt;set_errfile()</a>), the error message is written to that output stream.

- If none of these output options have been configured, the error message is written to stderr, the standard error output stream.

### Parameters

#### error

The **error** parameter is the error value for which the `DB_ENV->err()` and <a href="dberr.md" class="xref" title="DB-&gt;err()">DB-&gt;err()</a> methods will display a explanatory string.

#### fmt

The **fmt** parameter is an optional printf-style message to display.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
