---
title: "DB_ENV->memp_stat()"
api-name: "DB_ENV->memp_stat()"
source: docs/api_reference/C/mempstat.html
---
## DB_ENV-\>memp_stat()

``` c
#include <db.h>

int
DB_ENV->memp_stat(DB_ENV *env, DB_MPOOL_STAT **gsp,
    DB_MPOOL_FSTAT *(*fsp)[], u_int32_t flags);  
```

The `DB_ENV->memp_stat()` method returns the memory pool (that is, the buffer cache) subsystem statistics.

The `DB_ENV->memp_stat()` method creates statistical structures of type `DB_MPOOL_STAT` and `DB_MPOOL_FSTAT`, and copy pointers to them into user-specified memory locations. The cache statistics are stored in the `DB_MPOOL_STAT` structure and the per-file cache statistics are stored the `DB_MPOOL_FSTAT` structure.

Statistical structures are stored in allocated memory. If application-specific allocation routines have been declared (see <a href="envset_alloc.md" class="xref" title="DB_ENV-&gt;set_alloc()">DB_ENV-&gt;set_alloc()</a> for more information), they are used to allocate the memory; otherwise, the standard C library **malloc**(3) is used. The caller is responsible for deallocating the memory. To deallocate the memory, free the memory reference; references inside the returned memory need not be individually freed.

If **gsp** is non-NULL, the global statistics for the cache **mp** are copied into the memory location to which it refers. The following `DB_MPOOL_STAT` fields will be filled in:

- **u_int32_t st_gbytes;**

  Gigabytes of cache (total cache size is st_gbytes + st_bytes).

- **u_int32_t st_bytes;**

  Bytes of cache (total cache size is st_gbytes + st_bytes).

- **u_int32_t st_ncache;**

  Number of caches.

- **u_int32_t st_max_ncache;**

  Maximum number of caches, as configured with the <a href="envset_cache_max.md" class="xref" title="DB_ENV-&gt;set_cache_max()">DB_ENV-&gt;set_cache_max()</a> method.

- **roff_t st_regsize;**

  Individual cache size, in bytes.

- **size_t st_mmapsize;**

  Maximum memory-mapped file size.

- **int st_maxopenfd;**

  Maximum open file descriptors.

- **int st_maxwrite;**

  Maximum sequential buffer writes.

- **db_timeout_t st_maxwrite_sleep;**

  Microseconds to pause after writing maximum sequential buffers.

- **u_int32_t st_map;**

  Requested pages mapped into the process' address space (there is no available information about whether or not this request caused disk I/O, although examining the application page fault rate may be helpful).

- **uintmax_t st_cache_hit;**

  Requested pages found in the cache.

- **uintmax_t st_cache_miss;**

  Requested pages not found in the cache.

- **uintmax_t st_page_create;**

  Pages created in the cache.

- **uintmax_t st_page_in;**

  Pages read into the cache.

- **uintmax_t st_page_out;**

  Pages written from the cache to the backing file.

- **uintmax_t st_ro_evict;**

  Clean pages forced from the cache.

- **uintmax_t st_rw_evict;**

  Dirty pages forced from the cache.

- **uintmax_t st_page_trickle;**

  Dirty pages written using the <a href="memptrickle.md" class="xref" title="DB_ENV-&gt;memp_trickle()">DB_ENV-&gt;memp_trickle()</a> method.

- **u_int32_t st_pages;**

  Pages in the cache.

- **u_int32_t st_page_clean;**

  Clean pages currently in the cache.

- **u_int32_t st_page_dirty;**

  Dirty pages currently in the cache.

- **u_int32_t st_hash_buckets;**

  Number of hash buckets in buffer hash table.

- **uintmax_t st_hash_examined;**

  Total number of hash elements traversed during hash table lookups.

- **u_int32_t st_hash_longest;**

  Longest chain ever encountered in buffer hash table lookups.

- **u_int32_t st_hash_mutexes;**

  The number of hash bucket mutexes in the buffer hash table.

- **uintmax_t st_hash_nowait;**

  Number of times that a thread of control was able to obtain a hash bucket lock without waiting.

- **u_int32_t st_hash_searches;**

  Total number of buffer hash table lookups.

- **uintmax_t st_hash_wait;**

  Number of times that a thread of control was forced to wait before obtaining a hash bucket lock.

- **uintmax_t st_hash_max_nowait;**

  The number of times a thread of control was able to obtain the hash bucket lock without waiting on the bucket which had the maximum number of times that a thread of control needed to wait.

- **uintmax_t st_hash_max_wait;**

  Maximum number of times any hash bucket lock was waited for by a thread of control.

- **uintmax_t st_region_wait;**

  Number of times that a thread of control was forced to wait before obtaining a cache region mutex.

- **uintmax_t st_region_nowait;**

  Number of times that a thread of control was able to obtain a cache region mutex without waiting.

- **uintmax_t st_mvcc_frozen;**

  Number of buffers frozen.

- **uintmax_t st_mvcc_thawed;**

  Number of buffers thawed.

- **uintmax_t st_mvcc_freed;**

  Number of frozen buffers freed.

- **uintmax_t st_alloc;**

  Number of page allocations.

- **uintmax_t st_alloc_buckets;**

  Number of hash buckets checked during allocation.

- **uintmax_t st_alloc_max_buckets;**

  Maximum number of hash buckets checked during an allocation.

- **uintmax_t st_alloc_pages;**

  Number of pages checked during allocation.

- **uintmax_t st_alloc_max_pages;**

  Maximum number of pages checked during an allocation.

- **uintmax_t st_io_wait;**

  Number of operations blocked waiting for I/O to complete.

- **uintmax_t st_sync_interrupted;**

  Number of mpool sync operations interrupted.

If **fsp** is non-NULL, a pointer to a NULL-terminated variable length array of statistics for individual files, in the cache **mp**, is copied into the memory location to which it refers. If no individual files currently exist in the cache, **fsp** will be set to NULL.

The per-file statistics are stored in structures of type `DB_MPOOL_FSTAT`. The following `DB_MPOOL_FSTAT` fields will be filled in for each file in the cache; that is, each element of the array:

- **char \* file_name;**

  The name of the file.

- **size_t st_pagesize;**

  Page size in bytes.

- **uintmax_t st_cache_hit;**

  Requested pages found in the cache.

- **uintmax_t st_cache_miss;**

  Requested pages not found in the cache.

- **u_int32_t st_map;**

  Requested pages mapped into the process' address space.

- **uintmax_t st_page_create;**

  Pages created in the cache.

- **uintmax_t st_page_in;**

  Pages read into the cache.

- **uintmax_t st_page_out;**

  Pages written from the cache to the backing file.

The `DB_ENV->memp_stat()` method may not be called before the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called.

The `DB_ENV->memp_stat()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### gsp

The **gsp** parameter references memory into which a pointer to the allocated global statistics structure is copied.

#### fsp

The **fsp** parameter references memory into which a pointer to the allocated per-file statistics structures is copied.

#### flags

The **flags** parameter must be set to 0 or the following value:

- `DB_STAT_CLEAR`

  Reset statistics after returning their values.

### Errors

The `DB_ENV->memp_stat()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>
