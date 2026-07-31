---
title: "DB_ENV->mutex_stat()"
api-name: "DB_ENV->mutex_stat()"
source: docs/api_reference/C/mutexstat.html
---
## DB_ENV-\>mutex_stat()

``` c
#include <db.h>

int
DB_ENV->mutex_stat(DB_ENV *env, DB_MUTEX_STAT **statp, u_int32_t flags);  
```

The `DB_ENV->mutex_stat()` method returns the mutex subsystem statistics.

The `DB_ENV->mutex_stat()` method creates a statistical structure of type `DB_MUTEX_STAT` and copies a pointer to it into a user-specified memory location.

Statistical structures are stored in allocated memory. If application-specific allocation routines have been declared (see <a href="envset_alloc.md" class="xref" title="DB_ENV-&gt;set_alloc()">DB_ENV-&gt;set_alloc()</a> for more information), they are used to allocate the memory; otherwise, the standard C library **malloc**(3) is used. The caller is responsible for deallocating the memory. To deallocate the memory, free the memory reference; references inside the returned memory need not be individually freed.

The following `DB_MUTEX_STAT` fields will be filled in:

- **u_int32_t st_mutex_align;**

  The mutex alignment, in bytes.

- **int st_mutex_cnt;**

  The total number of mutexes configured.

- **u_int32_t st_mutex_free;**

  The number of mutexes currently available.

- **u_int32_t st_mutex_init;**

  The initial number of mutexes configured.

- **u_int32_t st_mutex_inuse;**

  The number of mutexes currently in use.

- **u_int32_t st_mutex_inuse_max;**

  The maximum number of mutexes ever in use.

- **u_int32_t st_mutex_max;**

  The maximum number of mutexes.

- **u_int32_t st_mutex_tas_spins;**

  The number of times test-and-set mutexes will spin without blocking.

- **uintmax_t st_region_wait;**

  The number of times that a thread of control was forced to wait before obtaining the mutex region mutex.

- **uintmax_t st_region_nowait;**

  The number of times that a thread of control was able to obtain the mutex region mutex without waiting.

- **roff_t st_regmax;**

  The max size of the mutex region size.

- **roff_t st_regsize;**

  The size of the mutex region, in bytes.

The `DB_ENV->mutex_stat()` method may not be called before the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called.

The `DB_ENV->mutex_stat()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### statp

The **statp** parameter references memory into which a pointer to the allocated statistics structure is copied.

#### flags

The **flags** parameter must be set to 0 or the following value:

- `DB_STAT_CLEAR`

  Reset statistics after returning their values.

### Errors

The `DB_ENV->mutex_stat()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="mutex.md#mutexlist" class="xref" title="Mutex Methods">Mutex Methods</a>
