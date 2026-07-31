---
title: "DB_ENV->mutex_alloc()"
api-name: "DB_ENV->mutex_alloc()"
source: docs/api_reference/C/mutexalloc.html
---
## DB_ENV-\>mutex_alloc()

``` c
#include <db.h>

int
DB_ENV->mutex_alloc(DB_ENV *dbenv, u_int32_t flags, db_mutex_t *mutexp);  
```

The `DB_ENV->mutex_alloc()` method allocates a mutex and returns a reference to it into the memory specified by **mutexp**.

The `DB_ENV->mutex_alloc()` method may not be called before the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called.

The `DB_ENV->mutex_alloc()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flags

The **flags** parameter must be set to 0 or by bitwise inclusively **OR**'ing together one or more of the following values:

- `DB_MUTEX_PROCESS_ONLY`

  The mutex is associated with a single process. The <a href="envfailchk.md" class="xref" title="DB_ENV-&gt;failchk()">DB_ENV-&gt;failchk()</a> method will release mutexes held by any process which has exited.

- `DB_MUTEX_SELF_BLOCK`

  The mutex must be self-blocking. That is, if a thread of control locks the mutex and then attempts to lock the mutex again, the thread of control will block until another thread of control releases the original lock on the mutex, allowing the original thread of control to lock the mutex the second time. Attempting to re-acquire a mutex for which the `DB_MUTEX_SELF_BLOCK` flag was not specified will result in undefined behavior.

#### mutexp

The **mutexp** parameter references memory into which the mutex reference is copied.

### Errors

The `DB_ENV->mutex_alloc()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="mutex.md#mutexlist" class="xref" title="Mutex Methods">Mutex Methods</a>
