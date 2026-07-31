---
title: "DB_ENV->set_isalive()"
api-name: "DB_ENV->set_isalive()"
source: docs/api_reference/C/envset_isalive.html
---
## DB_ENV-\>set_isalive()

``` c
#include <db.h>

int
DB_ENV->set_isalive(DB_ENV *dbenv, int (*is_alive)(DB_ENV *dbenv, 
    pid_t pid, db_threadid_t tid, u_int32_t flags));  
```

Declare a function that returns if a thread of control (either a true thread or a process) is still running. The `DB_ENV->set_isalive()` method supports the <a href="envfailchk.md" class="xref" title="DB_ENV-&gt;failchk()">DB_ENV-&gt;failchk()</a> method. For more information, see <a href="../../programmer_reference/cam_app.html" class="olink">Architecting Data Store and Concurrent Data Store applications</a>, and <a href="../../programmer_reference/transapp_app.html" class="olink">Architecting Transactional Data Store applications</a>, both in the *Berkeley DB Programmer's Reference Guide*.

The `DB_ENV->set_isalive()` method configures operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle, not all operations performed on the underlying database environment.

The `DB_ENV->set_isalive()` method may be called at any time during the life of the application.

The `DB_ENV->set_isalive()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### is_alive

The **is_alive** parameter is a function which returns non-zero if the thread of control, identified by the **pid** and **tid** arguments, is still running. The function takes four arguments:

- **dbenv**

  The **dbenv** parameter is the enclosing database environment handle, allowing application access to the application-private fields of that object.

- **pid**

  The **pid** parameter is a process ID returned by the function specified to the <a href="envset_thread_id.md" class="xref" title="DB_ENV-&gt;set_thread_id()">DB_ENV-&gt;set_thread_id()</a> method.

- **tid**

  The **tid** parameter is a thread ID returned by the function specified to the <a href="envset_thread_id.md" class="xref" title="DB_ENV-&gt;set_thread_id()">DB_ENV-&gt;set_thread_id()</a> method.

- **flags**

  The **flags** parameter must be set to 0 or the following value:

  - `DB_MUTEX_PROCESS_ONLY`

    Return only if the process is alive, the thread ID should be ignored.

### Errors

The `DB_ENV->set_isalive()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
