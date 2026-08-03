---
title: "DB_ENV->lock_stat()"
api-name: "DB_ENV->lock_stat()"
source: docs/api_reference/C/lockstat.html
---
## DB_ENV-\>lock_stat()

``` c
#include <db.h>

int
DB_ENV->lock_stat(DB_ENV *env, DB_LOCK_STAT **statp, u_int32_t flags);  
```

The `DB_ENV->lock_stat()` method returns the locking subsystem statistics.

The `DB_ENV->lock_stat()` method creates a statistical structure of type DB_LOCK_STAT and copies a pointer to it into a user-specified memory location.

Statistical structures are stored in allocated memory. If application-specific allocation routines have been declared (see <a href="envset_alloc.md" class="xref" title="DB_ENV-&gt;set_alloc()">DB_ENV-&gt;set_alloc()</a> for more information), they are used to allocate the memory; otherwise, the standard C library **malloc**(3) is used. The caller is responsible for deallocating the memory. To deallocate the memory, free the memory reference; references inside the returned memory need not be individually freed.

The following DB_LOCK_STAT fields will be filled in:

- **u_int32_t st_cur_maxid;**

  The current maximum unused locker ID.

- **u_int32_t st_hash_len;**

  Maximum length of a lock hash bucket.

- **u_int32_t st_id;**

  The last allocated locker ID.

- **u_int32_t st_initlocks;**

  The initial number of locks allocated in the lock table.

- **u_int32_t st_initlockers;**

  The initial number of lockers allocated in the lock table.

- **u_int32_t st_initobjects;**

  The initial number of lock objects allocated in the lock table.

- **uintmax_t st_lock_nowait;**

  The number of lock requests not immediately available due to conflicts, for which the thread of control did not wait.

- **uintmax_t st_lock_wait;**

  The number of lock requests not immediately available due to conflicts, for which the thread of control waited.

- **uintmax_t st_lockers_nowait;**

  The number of requests to allocate or deallocate a locker for which the thread of control did not wait.

- **uintmax_t st_lockers_wait;**

  The number of requests to allocate or deallocate a locker for which the thread of control waited.

- **u_int32_t st_lockers;**

  The current number of lockers allocated in the lock table.

- **u_int32_t st_locks;**

  The current number of locks allocated in the lock table.

- **uintmax_t st_locksteals;**

  The maximum number of locks stolen by an empty partition.

- **db_timeout_t st_locktimeout;**

  Lock timeout value.

- **u_int32_t st_maxhlocks;**

  The maximum number of locks in any hash bucket at any one time.

- **u_int32_t st_maxhobjects;**

  The maximum number of objects in any hash bucket at any one time.

- **u_int32_t st_maxlockers;**

  The maximum number of lockers possible.

- **u_int32_t st_maxlocks;**

  The maximum number of locks possible.

- **uintmax_t st_maxlsteals;**

  The maximum number of lock steals for any one partition.

- **u_int32_t st_maxnlockers;**

  The maximum number of lockers at any one time.

- **u_int32_t st_maxnobjects;**

  The maximum number of lock objects at any one time. Note that if there is more than one partition this is the sum of the maximum across all partitions.

- **u_int32_t st_maxnlocks;**

  The maximum number of locks at any one time. Note that if there is more than one partition, this is the sum of the maximum across all partitions.

- **u_int32_t st_maxobjects;**

  The maximum number of lock objects possible.

- **uintmax_t st_maxosteals;**

  The maximum number of object steals for any one partition.

- **uintmax_t st_ndeadlocks;**

  The number of deadlocks.

- **uintmax_t st_ndowngrade;**

  The total number of locks downgraded.

- **u_int32_t st_nlockers;**

  The number of current lockers.

- **u_int32_t st_nlocks;**

  The number of current locks.

- **uintmax_t st_nlocktimeouts;**

  The number of lock requests that have timed out.

- **int st_nmodes;**

  The number of lock modes.

- **u_int32_t st_nobjects;**

  The number of current lock objects.

- **uintmax_t st_nreleases;**

  The total number of locks released.

- **uintmax_t st_nrequests;**

  The total number of locks requested.

- **uintmax_t st_ntxntimeouts;**

  The number of transactions that have timed out. This value is also a component of **st_ndeadlocks**, the total number of deadlocks detected.

- **uintmax_t st_nupgrade;**

  The total number of locks upgraded.

- **u_int32_t st_objects;**

  The current number of lock objects allocated in the lock table.

- **uintmax_t st_objectsteals;**

  The maximum number of objects stolen by an empty partition.

- **uintmax_t st_objs_nowait;**

  The number of requests to allocate or deallocate an object for which the thread of control did not wait.

- **uintmax_t st_objs_wait;**

  The number of requests to allocate or deallocate an object for which the thread of control waited.

- **uintmax_t st_part_max_nowait;**

  The number of times that a thread of control was able to obtain any one lock partition mutex without waiting.

- **uintmax_t st_part_max_wait;**

  The maximum number of times that a thread of control was forced to wait before obtaining any one lock partition mutex.

- **uintmax_t st_part_nowait;**

  The number of times that a thread of control was able to obtain the lock partition mutex without waiting.

- **uintmax_t st_part_wait;**

  The number of times that a thread of control was forced to wait before obtaining the lock partition mutex.

- **u_int32_t st_partitions;**

  The number of lock table partitions.

- **uintmax_t st_region_nowait;**

  The number of times that a thread of control was able to obtain the lock region mutex without waiting.

- **uintmax_t st_region_wait;**

  The number of times that a thread of control was forced to wait before obtaining the lock region mutex.

- **roff_t st_regsize;**

  The region size, in bytes.

- **u_int32_t st_tablesize;**

  The size of the object hash table.

- **db_timeout_t st_txntimeout;**

  Transaction timeout value.

The `DB_ENV->lock_stat()` method may not be called before the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called.

The `DB_ENV->lock_stat()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### statp

The **statp** parameter references memory into which a pointer to the allocated statistics structure is copied.

#### flags

The **flags** parameter must be set to 0 or the following value:

- `DB_STAT_CLEAR`

  Reset statistics after returning their values.

### Errors

The `DB_ENV->lock_stat()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="lock.md" class="link" title="Chapter 6.  The DB_LOCK Handle">DB_LOCK</a>

### See Also

<a href="lock.md#locklist" class="xref" title="Locking Subsystem and Related Methods">Locking Subsystem and Related Methods</a>
