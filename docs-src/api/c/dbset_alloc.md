---
title: "DB->set_alloc()"
api-name: "DB->set_alloc()"
source: docs/api_reference/C/dbset_alloc.html
---
## DB-\>set_alloc()

``` c
#include <db.h>

int
DB->set_alloc(DB *db,
    void *(*app_malloc)(size_t),
    void *(*app_realloc)(void *, size_t),
    void (*app_free)(void *));  
```

Set the allocation functions used by the <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> and <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> methods to allocate or free memory owned by the application.

There are a number of interfaces in Berkeley DB where memory is allocated by the library and then given to the application. For example, the <a href="dbt.md#dbt_DB_DBT_MALLOC" class="link">DB_DBT_MALLOC</a> flag, when specified in the <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> object, will cause the <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> methods to allocate and reallocate memory which then becomes the responsibility of the calling application. (See <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> for more information.) Other examples are the Berkeley DB interfaces which return statistical information to the application: <a href="dbstat.md" class="xref" title="DB-&gt;stat()">DB-&gt;stat()</a>, <a href="lockstat.md" class="xref" title="DB_ENV-&gt;lock_stat()">DB_ENV-&gt;lock_stat()</a>, <a href="logarchive.md" class="xref" title="DB_ENV-&gt;log_archive()">DB_ENV-&gt;log_archive()</a>, <a href="logstat.md" class="xref" title="DB_ENV-&gt;log_stat()">DB_ENV-&gt;log_stat()</a>, <a href="mempstat.md" class="xref" title="DB_ENV-&gt;memp_stat()">DB_ENV-&gt;memp_stat()</a>, and <a href="txnstat.md" class="xref" title="DB_ENV-&gt;txn_stat()">DB_ENV-&gt;txn_stat()</a>. There is one method in Berkeley DB where memory is allocated by the application and then given to the library: <a href="dbassociate.md" class="xref" title="DB-&gt;associate()">DB-&gt;associate()</a>.

On systems in which there may be multiple library versions of the standard allocation routines (notably Windows NT), transferring memory between the library and the application will fail because the Berkeley DB library allocates memory from a different heap than the application uses to free it. To avoid this problem, the <a href="envset_alloc.md" class="xref" title="DB_ENV-&gt;set_alloc()">DB_ENV-&gt;set_alloc()</a> and `DB->set_alloc()` methods can be used to pass Berkeley DB references to the application's allocation routines.

It is not an error to specify only one or two of the possible allocation function parameters to these interfaces; however, in that case the specified interfaces must be compatible with the standard library interfaces, as they will be used together. The functions specified must match the calling conventions of the ANSI C X3.159-1989 (ANSI C) library routines of the same name.

Because databases opened within Berkeley DB environments use the allocation interfaces specified to the environment, it is an error to attempt to set those interfaces in a database created within an environment.

The `DB->set_alloc()` method may not be called after the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called.

The `DB->set_alloc()` method returns a non-zero error value on failure and 0 on success.

### Errors

The `DB->set_alloc()` method may fail and return one of the following non-zero errors:

#### EINVAL

If called in a database environment, or called after <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
