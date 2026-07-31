---
title: "DB_ENV->set_thread_count()"
api-name: "DB_ENV->set_thread_count()"
source: docs/api_reference/C/envset_thread_count.html
---
## DB_ENV-\>set_thread_count()

``` c
#include <db.h>

int
DB_ENV->set_thread_count(DB_ENV *dbenv, u_int32_t count);  
```

Declare an approximate number of threads in the database environment. This method allocates resources in your environment for the threads your application will use. If you fail to properly estimate the number of threads your application will use, your application will run out of resources and errors will be returned when the application attempts to start one too many threads.

The `DB_ENV->set_thread_count()` method does not set the maximum number of threads but is used to determine memory sizing and the thread control block reclamation policy.

The `DB_ENV->set_thread_count()` method must be called prior to opening the database environment. In addition, this method must be used with the <a href="envfailchk.md" class="xref" title="DB_ENV-&gt;failchk()">DB_ENV-&gt;failchk()</a> method.

If a process invokes this method without the use of <a href="envfailchk.md" class="xref" title="DB_ENV-&gt;failchk()">DB_ENV-&gt;failchk()</a> the program may be unable to allocate a thread control block. This is true of the standalone Berkeley DB utility programs.

If a process has not configured an **is_alive** function from the <a href="envset_isalive.md" class="xref" title="DB_ENV-&gt;set_isalive()">DB_ENV-&gt;set_isalive()</a> method, and then attempts to join a database environment configured for failure checking with the <a href="envfailchk.md" class="xref" title="DB_ENV-&gt;failchk()">DB_ENV-&gt;failchk()</a>, <a href="envset_thread_id.md" class="xref" title="DB_ENV-&gt;set_thread_id()">DB_ENV-&gt;set_thread_id()</a>, <a href="envset_isalive.md" class="xref" title="DB_ENV-&gt;set_isalive()">DB_ENV-&gt;set_isalive()</a> and `DB_ENV->set_thread_count()` methods, the program may be unable to allocate a thread control block and fail to join the environment. **This is true of the standalone Berkeley DB utility programs.** To avoid problems when using the standalone Berkeley DB utility programs with environments configured for failure checking, incorporate the utility's functionality directly in the application, or call the <a href="envfailchk.md" class="xref" title="DB_ENV-&gt;failchk()">DB_ENV-&gt;failchk()</a> method before running the utility.

The database environment's thread count may also be configured using the environment's <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "set_thread_count", one or more whitespace characters, and the thread count. Because the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

The `DB_ENV->set_thread_count()` method configures operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle, not all operations performed on the underlying database environment.

The `DB_ENV->set_thread_count()` method may not be called after the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called.

The `DB_ENV->set_thread_count()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### count

The **count** parameter is an approximate thread count for the database environment.

### Errors

The `DB_ENV->set_thread_count()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
