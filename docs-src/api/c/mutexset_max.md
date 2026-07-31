---
title: "DB_ENV->mutex_set_max()"
api-name: "DB_ENV->mutex_set_max()"
source: docs/api_reference/C/mutexset_max.html
---
## DB_ENV-\>mutex_set_max()

``` c
#include <db.h>

int
DB_ENV->mutex_set_max(DB_ENV *dbenv, u_int32_t max);  
```

Configure the total number of mutexes to allocate. This method is deprecated. The maximum size of the mutex region is now inferred by the sizes of the other memory structures, and so this method is no longer needed. For example, the cache requires one mutex per page in the cache. When you specify the cache size, DB assumes a page size of 4K and allocates mutexes accordingly. If your page size is different than 4K, you indicate this using <a href="envset_mp_pagesize.md" class="xref" title="DB_ENV-&gt;set_mp_pagesize()">DB_ENV-&gt;set_mp_pagesize()</a>. DB will then allocate the proper number of mutexes based on this new page size.

You can use this method to override DB's mutex calculation, but it is not recommended to do so.

Calling the `DB_ENV->mutex_set_max()` method discards any value previously set using the <a href="mutexset_increment.md" class="xref" title="DB_ENV-&gt;mutex_set_increment()">DB_ENV-&gt;mutex_set_increment()</a> method.

The database environment's total number of mutexes may also be configured using the environment's <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "mutex_set_max", one or more whitespace characters, and the total number of mutexes. Because the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

The `DB_ENV->mutex_set_max()` method configures a database environment, not only operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle.

The `DB_ENV->mutex_set_max()` method may not be called after the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called. If the database environment already exists when <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> is called, the information specified to `DB_ENV->mutex_set_max()` will be ignored.

The `DB_ENV->mutex_set_max()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### max

The **max** parameter is the absolute number of mutexes to allocate.

### Errors

The `DB_ENV->mutex_set_max()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="mutex.md#mutexlist" class="xref" title="Mutex Methods">Mutex Methods</a>
