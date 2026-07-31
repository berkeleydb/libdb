---
title: "DB_ENV->mutex_set_tas_spins()"
api-name: "DB_ENV->mutex_set_tas_spins()"
source: docs/api_reference/C/mutexset_tas_spins.html
---
## DB_ENV-\>mutex_set_tas_spins()

``` c
#include <db.h>

int
DB_ENV->mutex_set_tas_spins(DB_ENV *dbenv, u_int32_t tas_spins);  
```

Specify that test-and-set mutexes should spin **tas_spins** times without blocking. The value defaults to 1 on uniprocessor systems and to 50 times the number of processors on multiprocessor systems.

The database environment's test-and-set spin count may also be configured using the environment's <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "set_tas_spins", one or more whitespace characters, and the number of spins. Because the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

The `DB_ENV->mutex_set_tas_spins()` method configures operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle, not all operations performed on the underlying database environment.

The `DB_ENV->mutex_set_tas_spins()` method may be called at any time during the life of the application.

The `DB_ENV->mutex_set_tas_spins()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### tas_spins

The **tas_spins** parameter is the number of spins test-and-set mutexes should execute before blocking.

### Errors

The `DB_ENV->mutex_set_tas_spins()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="mutex.md#mutexlist" class="xref" title="Mutex Methods">Mutex Methods</a>
