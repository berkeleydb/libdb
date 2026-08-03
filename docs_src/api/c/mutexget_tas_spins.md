---
title: "DB_ENV->mutex_get_tas_spins()"
api-name: "DB_ENV->mutex_get_tas_spins()"
source: docs/api_reference/C/mutexget_tas_spins.html
---
## DB_ENV-\>mutex_get_tas_spins()

``` c
#include <db.h>

int
DB_ENV->mutex_get_tas_spins(DB_ENV *dbenv, u_int32_t *tas_spinsp);  
```

The `DB_ENV->mutex_get_tas_spins()` method returns the test-and-set spin count. This value may be configured using the <a href="mutexset_tas_spins.md" class="xref" title="DB_ENV-&gt;mutex_set_tas_spins()">DB_ENV-&gt;mutex_set_tas_spins()</a> method.

The `DB_ENV->mutex_get_tas_spins()` method may be called at any time during the life of the application.

The `DB_ENV->mutex_get_tas_spins()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### tas_spinsp

The `DB_ENV->mutex_get_tas_spins()` method returns the test-and-set spin count in **tas_spinsp**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="mutex.md#mutexlist" class="xref" title="Mutex Methods">Mutex Methods</a>
