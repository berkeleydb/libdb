---
title: "DB->set_cachesize()"
api-name: "DB->set_cachesize()"
source: docs/api_reference/C/dbset_cachesize.html
---
## DB-\>set_cachesize()

``` c
#include <db.h>

int
DB->set_cachesize(DB *db,
    u_int32_t gbytes, u_int32_t bytes, int ncache);  
```

Set the size of the shared memory buffer pool -- that is, the cache. The cache should be the size of the normal working data set of the application, with some small amount of additional memory for unusual situations. (Note: the working set is not the same as the number of pages accessed simultaneously, and is usually much larger.)

The default cache size is 256KB, and may not be specified as less than 20KB. Any cache size less than 500MB is automatically increased by 25% to account for buffer pool overhead; cache sizes larger than 500MB are used as specified. The maximum size of a single cache is 4GB on 32-bit systems and 10TB on 64-bit systems. (All sizes are in powers-of-two, that is, 256KB is 2^18 not 256,000.) For information on tuning the Berkeley DB cache size, see <a href="../../programmer_reference/general_am_conf.html#am_conf_cachesize" class="olink">Selecting a cache size</a>.

It is possible to specify caches to Berkeley DB large enough they cannot be allocated contiguously on some architectures. For example, some releases of Solaris limit the amount of memory that may be allocated contiguously by a process. If **ncache** is 0 or 1, the cache will be allocated contiguously in memory. If it is greater than 1, the cache will be split across **ncache** separate regions, where the **region size** is equal to the initial cache size divided by **ncache**.

Because databases opened within Berkeley DB environments use the cache specified to the environment, it is an error to attempt to set a cache in a database created within an environment.

The `DB->set_cachesize()` method may not be called after the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called.

The `DB->set_cachesize()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### gbytes

The size of the cache is set to **gbytes** gigabytes plus **bytes**.

#### bytes

The size of the cache is set to **gbytes** gigabytes plus **bytes**.

#### ncache

The **ncache** parameter is the number of caches to create.

### Errors

The `DB->set_cachesize()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the specified cache size was impossibly small; the method was called after <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
