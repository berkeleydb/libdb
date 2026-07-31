---
title: "DB_ENV->mutex_get_max()"
api-name: "DB_ENV->mutex_get_max()"
source: docs/api_reference/C/mutexget_max.html
---
## DB_ENV-\>mutex_get_max()

``` c
#include <db.h>

int
DB_ENV->mutex_get_max(DB_ENV *dbenv, u_int32_t *maxp);  
```

The `DB_ENV->mutex_get_max()` method returns the total number of mutexes allocated. This method is deprecated.

The `DB_ENV->mutex_get_max()` method may be called at any time during the life of the application.

The `DB_ENV->mutex_get_max()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### maxp

The `DB_ENV->mutex_get_max()` method returns the total number of mutexes allocated in **maxp**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="mutex.md#mutexlist" class="xref" title="Mutex Methods">Mutex Methods</a>
