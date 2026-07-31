---
title: "DB_ENV->mutex_unlock()"
api-name: "DB_ENV->mutex_unlock()"
source: docs/api_reference/C/mutexunlock.html
---
## DB_ENV-\>mutex_unlock()

``` c
#include <db.h>

int
DB_ENV->mutex_unlock(DB_ENV *dbenv, db_mutex_t mutex);  
```

The `DB_ENV->mutex_unlock()` method unlocks the mutex locked by <a href="mutexlock.md" class="xref" title="DB_ENV-&gt;mutex_lock()">DB_ENV-&gt;mutex_lock()</a>.

The `DB_ENV->mutex_unlock()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### mutex

The **mutex** parameter is a mutex previously locked by <a href="mutexlock.md" class="xref" title="DB_ENV-&gt;mutex_lock()">DB_ENV-&gt;mutex_lock()</a>.

### Errors

The `DB_ENV->mutex_unlock()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="mutex.md#mutexlist" class="xref" title="Mutex Methods">Mutex Methods</a>
