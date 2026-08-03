---
title: "DB_ENV->mutex_get_increment()"
api-name: "DB_ENV->mutex_get_increment()"
source: docs/api_reference/C/mutexget_increment.html
---
## DB_ENV-\>mutex_get_increment()

``` c
#include <db.h>

int
DB_ENV->mutex_get_increment(DB_ENV *dbenv, u_int32_t *incrementp);  
```

The `DB_ENV->mutex_get_increment()` method returns the number of additional mutexes to allocate.

The `DB_ENV->mutex_get_increment()` method may be called at any time during the life of the application.

The `DB_ENV->mutex_get_increment()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### incrementp

The `DB_ENV->mutex_get_increment()` method returns the number of additional mutexes to allocate in **incrementp**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="mutex.md#mutexlist" class="xref" title="Mutex Methods">Mutex Methods</a>
