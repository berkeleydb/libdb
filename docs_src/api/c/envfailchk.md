---
title: "DB_ENV->failchk()"
api-name: "DB_ENV->failchk()"
source: docs/api_reference/C/envfailchk.html
---
## DB_ENV-\>failchk()

``` c
#include <db.h>

int
DB_ENV->failchk(DB_ENV *dbenv, u_int32_t flags);  
```

The `DB_ENV->failchk()` method checks for threads of control (either a true thread or a process) that have exited while manipulating Berkeley DB library data structures, while holding a logical database lock, or with an unresolved transaction (that is, a transaction that was never aborted or committed). For more information, see <a href="../../guides/programmer_reference/cam_app.md" class="olink">Architecting Data Store and Concurrent Data Store applications</a>, and <a href="../../guides/programmer_reference/transapp_app.md" class="olink">Architecting Transactional Data Store applications</a>, both in the *Berkeley DB Programmer's Reference Guide*.

The `DB_ENV->failchk()` method is used in conjunction with the <a href="envset_thread_count.md" class="xref" title="DB_ENV-&gt;set_thread_count()">DB_ENV-&gt;set_thread_count()</a>, <a href="envset_isalive.md" class="xref" title="DB_ENV-&gt;set_isalive()">DB_ENV-&gt;set_isalive()</a> and <a href="envset_thread_id.md" class="xref" title="DB_ENV-&gt;set_thread_id()">DB_ENV-&gt;set_thread_id()</a> methods. Before calling the `failchk()`method, applications must:

1.  Configure their database using the <a href="envset_thread_count.md" class="xref" title="DB_ENV-&gt;set_thread_count()">DB_ENV-&gt;set_thread_count()</a> method.

2.  Establish an `is_alive()` function and invoke <a href="envset_isalive.md" class="xref" title="DB_ENV-&gt;set_isalive()">DB_ENV-&gt;set_isalive()</a> with that function as the `is_alive` parameter.

3.  Establish a `thread_id` function and invoke <a href="envset_thread_id.md" class="xref" title="DB_ENV-&gt;set_thread_id()">DB_ENV-&gt;set_thread_id()</a> with that function as the `thread_id` parameter.

If any of these methods are omitted, a program may be unable to allocate a thread control block. This is true of the standalone Berkeley DB utility programs. To avoid problems when using the standalone Berkeley DB utility programs with environments configured for failure checking, incorporate the utility's functionality directly in the application, or call the `DB_ENV->failchk()` method along with its associated methods before running the utility.

If `DB_ENV->failchk()` determines a thread of control exited while holding database read locks, it will release those locks. If `DB_ENV->failchk()` determines a thread of control exited with an unresolved transaction, the transaction will be aborted. In either of these cases, `DB_ENV->failchk()` will return 0 and the application may continue to use the database environment.

In either of these cases, the `DB_ENV->failchk()` method will also report the process and thread IDs associated with any released locks or aborted transactions. The information is printed to a specified output channel (see the <a href="envset_msgfile.md" class="xref" title="DB_ENV-&gt;set_msgfile()">DB_ENV-&gt;set_msgfile()</a> method for more information), or passed to an application callback function (see the <a href="envset_msgcall.md" class="xref" title="DB_ENV-&gt;set_msgcall()">DB_ENV-&gt;set_msgcall()</a> method for more information).

If `DB_ENV->failchk()` determines a thread of control has exited such that database environment recovery is required, it will return <a href="../../guides/programmer_reference/program_errorret.md#program_errorret.DB_RUNRECOVERY" class="olink">DB_RUNRECOVERY</a>. In this case, the application should not continue to use the database environment. For a further description as to the actions the application should take when this failure occurs, see <a href="../../guides/programmer_reference/cam_fail.md" class="olink">Handling failure in Data Store and Concurrent Data Store applications</a>, and <a href="../../guides/programmer_reference/transapp_fail.md" class="olink">Handling failure in Transactional Data Store applications</a>, both in the *Berkeley DB Programmer's Reference Guide*.

In multiprocess applications, it is recommended that the <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle used to invoke the `DB_ENV->failchk()` method not be shared and therefore not <span class="emphasis">*free-threaded*</span>.

The `DB_ENV->failchk()` method may not be called by the application before the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called.

The `DB_ENV->failchk()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flags

The **flags** parameter is currently unused, and must be set to 0.

### Errors

The `DB_ENV->failchk()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
