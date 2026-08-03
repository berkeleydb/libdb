---
title: "DB_ENV->set_thread_id()"
api-name: "DB_ENV->set_thread_id()"
source: docs/api_reference/C/envset_thread_id.html
---
## DB_ENV-\>set_thread_id()

``` c
#include <db.h>

int
DB_ENV->set_thread_id(DB_ENV *dbenv,
    void (*thread_id)(DB_ENV *dbenv, pid_t *pid, db_threadid_t *tid));  
```

Declare a function that returns a unique identifier pair for the current thread of control. The `DB_ENV->set_thread_id()` method supports the <a href="envfailchk.md" class="xref" title="DB_ENV-&gt;failchk()">DB_ENV-&gt;failchk()</a> method. For more information, see <a href="../../guides/programmer_reference/cam_app.md" class="olink">Architecting Data Store and Concurrent Data Store applications</a> , and <a href="../../guides/programmer_reference/transapp_app.md" class="olink">Architecting Transactional Data Store applications</a> , both in the *Berkeley DB Programmer's Reference Guide*.

The `DB_ENV->set_thread_id()` method configures operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle, not all operations performed on the underlying database environment.

The `DB_ENV->set_thread_id()` method may be called at any time during the life of the application.

The `DB_ENV->set_thread_id()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### thread_id

The **thread_id** parameter is a function which returns a unique identifier pair for a thread of control in a Berkeley DB application. The function takes three arguments:

- **dbenv**

  The **dbenv** parameter is the enclosing database environment handle, allowing application access to the application-private fields of that object.

- **pid**

  The **pid** points to a memory location of type `pid_t`, or NULL. The process ID of the current thread of control may be returned in this memory location, if it is not NULL.

- **tid**

  The **tid** points to a memory location of type `db_threadid_t`, or NULL. The thread ID of the current thread of control may be returned in this memory location, if it is not NULL.

### Errors

The `DB_ENV->set_thread_id()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Assigning Thread IDs

The standard system library calls to return process and thread IDs are often sufficient for this purpose (for example, `getpid()` and `pthread_self()` on POSIX systems or GetCurrentThreadID on Windows systems). However, if the Berkeley DB application dynamically creates processes or threads, some care may be necessary in assigning unique IDs. In most threading systems, process and thread IDs are available for re-use as soon as the process or thread exits. If a new process or thread is created between the time of process or thread exit, and the <a href="envfailchk.md" class="xref" title="DB_ENV-&gt;failchk()">DB_ENV-&gt;failchk()</a> method is run, it may be possible for <a href="envfailchk.md" class="xref" title="DB_ENV-&gt;failchk()">DB_ENV-&gt;failchk()</a> to not detect that a thread of control exited without properly releasing all Berkeley DB resources.

It may be possible to handle this problem by inhibiting process or thread creation between thread of control exit and calling the <a href="envfailchk.md" class="xref" title="DB_ENV-&gt;failchk()">DB_ENV-&gt;failchk()</a> method. Alternatively, the **thread_id** function must be constructed to not re-use **pid/tid** pairs. For example, in a single process application, the returned process ID might be used as an incremental counter, with the returned thread ID set to the actual thread ID. Obviously, the **is_alive** function specified to the <a href="envset_isalive.md" class="xref" title="DB_ENV-&gt;set_isalive()">DB_ENV-&gt;set_isalive()</a> method must be compatible with any **thread_id** function specified to `DB_ENV->set_thread_id()`.

The db_threadid_t type is configured to be the same type as a standard thread identifier, in Berkeley DB configurations where this type is known (for example, systems supporting pthread_t or thread_t, or DWORD on Windows). If the Berkeley DB configuration process is unable to determine the type of a standard thread identifier, the db_thread_t type is set to uintmax_t (or the largest available unsigned integral type, on systems lacking the uintmax_t type). Applications running on systems lacking a detectable standard thread type, and which are also using thread APIs where a thread identifier is not an integral value and so will not fit into the configured db_threadid_t type, must either translate between the db_threadid_t type and the thread identifier (mapping the thread identifier to a unique identifier of the appropriate size), or modify the Berkeley DB sources to use an appropriate db_threadid_t type. Note: we do not currently know of any systems where this is necessary. If your application has to solve this problem, please contact our support group and let us know.

If no **thread_id** function is specified by the application, the Berkeley DB library will identify threads of control by using the `taskIdSelf()` call on VxWorks, the `getpid()` and `GetCurrentThreadID()` calls on Windows, the `getpid()` and `pthread_self()` calls when the Berkeley DB library has been configured for POSIX pthreads or Solaris LWP threads, the `getpid()` and `thr_self()` calls when the Berkeley DB library has been configured for UI threads, and otherwise `getpid()`.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
