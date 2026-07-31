---
title: "DB_ENV->mutex_set_init()"
api-name: "DB_ENV->mutex_set_init()"
source: docs/api_reference/C/mutexset_init.html
---
## DB_ENV-\>mutex_set_init()

``` c
#include <db.h>

int
DB_ENV->mutex_set_init(DB_ENV *dbenv, u_int32_t init); 
```

Configure the inital number of mutexes to allocate.

Berkeley DB allocates a default number of mutexes based on the initial configuration of the database environment. The `DB_ENV->mutex_set_init()` method is used to override this default number of mutexes to allocate. This may be done to either speed up startup, or to force more work to be done at startup to avoid later contention due to allocation.

The database environment's inital number of mutexes may also be configured using the environment's <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "mutex_set_init", one or more whitespace characters, and the initial number of mutexes. Because the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

The `DB_ENV->mutex_set_init()` method configures a database environment, not only operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle.

The `DB_ENV->mutex_set_init()` method may not be called after the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called. If the database environment already exists when <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> is called, the information specified to `DB_ENV->mutex_set_init()` will be ignored.

The `DB_ENV->mutex_set_init()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### init

The **init** parameter is the absolute number of mutexes to allocate.

### Errors

The `DB_ENV->mutex_set_init()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="mutex.md#mutexlist" class="xref" title="Mutex Methods">Mutex Methods</a>
