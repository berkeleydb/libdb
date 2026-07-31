---
title: "DB_ENV->set_cachesize()"
api-name: "DB_ENV->set_cachesize()"
source: docs/api_reference/C/envset_cachesize.html
---
## DB_ENV-\>set_cachesize()

``` c
#include <db.h>

int
DB_ENV->set_cachesize(DB_ENV *dbenv,
    u_int32_t gbytes, u_int32_t bytes, int ncache);  
```

Sets the size of the shared memory buffer pool — that is, the cache. The cache should be the size of the normal working data set of the application, with some small amount of additional memory for unusual situations. (Note: the working set is not the same as the number of pages accessed simultaneously, and is usually much larger.)

The default cache size is 256KB, and may not be specified as less than 20KB. Any cache size less than 500MB is automatically increased by 25% to account for cache overhead; cache sizes larger than 500MB are used as specified. The maximum size of a single cache is 4GB on 32-bit systems and 10TB on 64-bit systems. (All sizes are in powers-of-two, that is, 256KB is 2^18 not 256,000.) For information on tuning the Berkeley DB cache size, see <a href="../../guides/programmer_reference/general_am_conf.md#am_conf_cachesize" class="olink">Selecting a cache size</a>.

It is possible to specify caches to Berkeley DB large enough they cannot be allocated contiguously on some architectures. For example, some releases of Solaris limit the amount of memory that may be allocated contiguously by a process. If **ncache** is 0 or 1, the cache will be allocated contiguously in memory. If it is greater than 1, the cache will be split across **ncache** separate regions, where the **region size** is equal to the initial cache size divided by **ncache**.

The cache may be resized by calling `DB_ENV->set_cachesize()` after the environment is open. The supplied size will be rounded to the nearest multiple of the region size and may not be larger than the maximum size configured with <a href="envset_cache_max.md" class="xref" title="DB_ENV-&gt;set_cache_max()">DB_ENV-&gt;set_cache_max()</a>. The **ncache** parameter is ignored when resizing the cache.

The database environment's initial cache size may also be configured using the environment's <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "set_cachesize", one or more whitespace characters, and the initial cache size specified in three parts: the gigabytes of cache, the additional bytes of cache, and the number of caches, also separated by whitespace characters. For example, "set_cachesize 2 524288000 3" would create a 2.5GB logical cache, split between three physical caches. Because the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

The `DB_ENV->set_cachesize()` method configures a database environment, not only operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle.

The `DB_ENV->set_cachesize()` method may be called at any time during the life of the application.

The `DB_ENV->set_cachesize()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### gbytes

The size of the cache is set to **gbytes** gigabytes plus **bytes**.

#### bytes

The size of the cache is set to **gbytes** gigabytes plus **bytes**.

#### ncache

The **ncache** parameter is the number of caches to create.

### Errors

The `DB_ENV->set_cachesize()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the specified cache size was impossibly small; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
