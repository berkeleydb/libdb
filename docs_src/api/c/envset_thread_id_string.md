---
title: "DB_ENV->set_thread_id_string()"
api-name: "DB_ENV->set_thread_id_string()"
source: docs/api_reference/C/envset_thread_id_string.html
---
## DB_ENV-\>set_thread_id_string()

``` c
#include <db.h>

int
DB_ENV->set_thread_id_string(DB_ENV *dbenv,
    char *(*thread_id_string)(DB_ENV *dbenv,
    pid_t pid, db_threadid_t tid, char *buf));  
```

Declare a function that formats a process ID and thread ID identifier pair for display into a caller-supplied buffer. The function must return a reference to the caller-specified buffer. The `DB_ENV->set_thread_id_string()` method supports the <a href="envset_thread_id.md" class="xref" title="DB_ENV-&gt;set_thread_id()">DB_ENV-&gt;set_thread_id()</a> method.

The `DB_ENV->set_thread_id_string()` method configures operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle, not all operations performed on the underlying database environment.

The `DB_ENV->set_thread_id_string()` method may be called at any time during the life of the application.

The `DB_ENV->set_thread_id_string()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### thread_id_string

The **thread_id_string** parameter is a function which returns a buffer in which is an identifier pair formatted for display. The function takes four arguments:

- `dbenv`

  The **dbenv** parameter is the enclosing database environment handle, allowing application access to the application-private fields of that object.

- `pid`

  The **pid** argument is a process ID.

- `tid`

  The **tid** argument is a thread ID.

- `buf`

  The **buf** argument is character array of at least DB_THREADID_STRLEN bytes in length, into which the identifier pair should be formatted.

If no **thread_id_string** function is specified, the default routine displays the identifier pair as "pid/tid", that is, the process ID represented as an unsigned integer value, a slash ('/') character, then the thread ID represented as an unsigned integer value.

### Errors

The `DB_ENV->set_thread_id_string()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
