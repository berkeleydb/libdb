---
title: "DB_ENV->mutex_free()"
api-name: "DB_ENV->mutex_free()"
source: docs/api_reference/C/mutexfree.html
---
## DB_ENV-\>mutex_free()

``` c
#include <db.h>

int
DB_ENV->mutex_free(DB_ENV *dbenv, db_mutex_t mutex);  
```

The `DB_ENV->mutex_free()` method discards a mutex allocated by <a href="mutexalloc.md" class="xref" title="DB_ENV-&gt;mutex_alloc()">DB_ENV-&gt;mutex_alloc()</a>.

The `DB_ENV->mutex_free()` method may not be called before the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called.

The `DB_ENV->mutex_free()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### mutex

The **mutex** parameter is a mutex previously allocated by <a href="mutexalloc.md" class="xref" title="DB_ENV-&gt;mutex_alloc()">DB_ENV-&gt;mutex_alloc()</a>.

### Errors

The `DB_ENV->mutex_free()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="mutex.md#mutexlist" class="xref" title="Mutex Methods">Mutex Methods</a>
