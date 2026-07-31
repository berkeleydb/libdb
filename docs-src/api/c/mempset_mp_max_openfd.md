---
title: "DB_ENV->set_mp_max_openfd()"
api-name: "DB_ENV->set_mp_max_openfd()"
source: docs/api_reference/C/mempset_mp_max_openfd.html
---
## DB_ENV-\>set_mp_max_openfd()

``` c
#include <db.h>

int
DB_ENV->set_mp_max_openfd(DB_ENV *env, int maxopenfd);  
```

The `DB_ENV->set_mp_max_openfd()` method limits the number of file descriptors the library will open concurrently when flushing dirty pages from the cache.

The database environment's limit on open file descriptors to flush dirty pages may also be configured using the environment's <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "set_mp_max_openfd", one or more whitespace characters, and the number of open file descriptors. Because the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

The <a href="mempset_mp_max_openfd.md" class="xref" title="DB_ENV-&gt;set_mp_max_openfd()">DB_ENV-&gt;set_mp_max_openfd()</a> method configures a database environment, not only operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle.

The `DB_ENV->set_mp_max_openfd()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### maxopenfd

The maximum number of file descriptors that may be concurrently opened by the library when flushing dirty pages from the cache.

### Errors

The `DB_ENV->set_mp_max_openfd()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>
