---
title: "DB_ENV->mutex_get_align()"
api-name: "DB_ENV->mutex_get_align()"
source: docs/api_reference/C/mutexget_align.html
---
## DB_ENV-\>mutex_get_align()

``` c
#include <db.h>

int
DB_ENV->mutex_get_align(DB_ENV *dbenv, u_int32_t *alignp);  
```

The `DB_ENV->mutex_get_align()` method returns the mutex alignment, in bytes.

The `DB_ENV->mutex_get_align()` method may be called at any time during the life of the application.

The `DB_ENV->mutex_get_align()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### alignp

The `DB_ENV->mutex_get_align()` method returns the mutex alignment, in bytes in **alignp**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="mutex.md#mutexlist" class="xref" title="Mutex Methods">Mutex Methods</a>
