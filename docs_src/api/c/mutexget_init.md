---
title: "DB_ENV->mutex_get_init()"
api-name: "DB_ENV->mutex_get_init()"
source: docs/api_reference/C/mutexget_init.html
---
## DB_ENV-\>mutex_get_init()

``` c
#include <db.h>
int
DB_ENV->mutex_get_init(DB_ENV *dbenv, u_int32_t *init);  
```

The `DB_ENV->mutex_get_init()` method returns the inital number of mutexes allocated. This value can be set using the <a href="mutexset_init.md" class="xref" title="DB_ENV-&gt;mutex_set_init()">DB_ENV-&gt;mutex_set_init()</a> method.

The `DB_ENV->mutex_get_init()` method may be called at any time during the life of the application.

The `DB_ENV->mutex_get_init()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### init

The `DB_ENV->mutex_get_init()` method returns the inital number of mutexes allocated in **init**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="mutex.md#mutexlist" class="xref" title="Mutex Methods">Mutex Methods</a>
