---
title: "DB_ENV->mutex_set_increment()"
api-name: "DB_ENV->mutex_set_increment()"
source: docs/api_reference/C/mutexset_increment.html
---
## DB_ENV-\>mutex_set_increment()

``` c
#include <db.h>

int
DB_ENV->mutex_set_increment(DB_ENV *dbenv, u_int32_t increment);  
```

Configure the number of additional mutexes to allocate.

If an application will allocate mutexes for its own use, the `DB_ENV->mutex_set_increment()` method is used to add a number of mutexes to the default allocation.

Calling the `DB_ENV->mutex_set_increment()` method discards any value previously set using the <a href="mutexset_max.md" class="xref" title="DB_ENV-&gt;mutex_set_max()">DB_ENV-&gt;mutex_set_max()</a> method.

The database environment's number of additional mutexes may also be configured using the environment's <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "mutex_set_increment", one or more whitespace characters, and the number of additional mutexes. Because the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

The `DB_ENV->mutex_set_increment()` method configures a database environment, not only operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle.

The `DB_ENV->mutex_set_increment()` method may not be called after the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called. If the database environment already exists when <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> is called, the information specified to `DB_ENV->mutex_set_increment()` will be ignored.

The `DB_ENV->mutex_set_increment()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### increment

The **increment** parameter is the number of additional mutexes to allocate.

### Errors

The `DB_ENV->mutex_set_increment()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="mutex.md#mutexlist" class="xref" title="Mutex Methods">Mutex Methods</a>
