---
title: "DB_ENV->set_alloc()"
api-name: "DB_ENV->set_alloc()"
source: docs/api_reference/C/envset_alloc.html
---
## DB_ENV-\>set_alloc()

``` c
#include <db.h>

int
DB_ENV->set_alloc(DB_ENV *dbenv,
    void *(*app_malloc)(size_t),
    void *(*app_realloc)(void *, size_t),
    void (*app_free)(void *));  
```

Set the allocation functions used by the <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> and <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> methods to allocate or free memory owned by the application.

There are a number of interfaces in Berkeley DB where memory is allocated by the library and then given to the application. For example, the <a href="dbt.md#dbt_DB_DBT_MALLOC" class="link">DB_DBT_MALLOC</a> flag, when specified in the <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> object, will cause the <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> methods to allocate and reallocate memory which then becomes the responsibility of the calling application. Other examples are the Berkeley DB interfaces which return statistical information to the application: <a href="dbstat.md" class="xref" title="DB-&gt;stat()">DB-&gt;stat()</a>, <a href="lockstat.md" class="xref" title="DB_ENV-&gt;lock_stat()">DB_ENV-&gt;lock_stat()</a>, <a href="logarchive.md" class="xref" title="DB_ENV-&gt;log_archive()">DB_ENV-&gt;log_archive()</a>, <a href="logstat.md" class="xref" title="DB_ENV-&gt;log_stat()">DB_ENV-&gt;log_stat()</a>, <a href="mempstat.md" class="xref" title="DB_ENV-&gt;memp_stat()">DB_ENV-&gt;memp_stat()</a>, and <a href="txnstat.md" class="xref" title="DB_ENV-&gt;txn_stat()">DB_ENV-&gt;txn_stat()</a>. There is one method in Berkeley DB where memory is allocated by the application and then given to the library: the callback specified to <a href="dbassociate.md" class="xref" title="DB-&gt;associate()">DB-&gt;associate()</a>.

On systems in which there may be multiple library versions of the standard allocation routines (notably Windows NT), transferring memory between the library and the application will fail because the Berkeley DB library allocates memory from a different heap than the application uses to free it. To avoid this problem, the `DB_ENV->set_alloc()` and <a href="dbset_alloc.md" class="xref" title="DB-&gt;set_alloc()">DB-&gt;set_alloc()</a> methods can be used to pass Berkeley DB references to the application's allocation routines.

It is not an error to specify only one or two of the possible allocation function parameters to these interfaces; however, in that case the specified interfaces must be compatible with the standard library interfaces, as they will be used together. The functions specified must match the calling conventions of the ANSI C X3.159-1989 (ANSI C) library routines of the same name.

The `DB_ENV->set_alloc()` method configures operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle, not all operations performed on the underlying database environment.

The `DB_ENV->set_alloc()` method may not be called after the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called.

The `DB_ENV->set_alloc()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### app_malloc

The **app_malloc** parameter is the application-specified malloc function.

#### app_realloc

The **app_realloc** parameter is the application-specified realloc function.

#### app_free

The **app_free** parameter is the application-specified free function.

### Errors

The `DB_ENV->set_alloc()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
