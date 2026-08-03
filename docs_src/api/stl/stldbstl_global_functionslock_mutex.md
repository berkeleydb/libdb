---
title: "lock_mutex"
api-name: "lock_mutex"
source: docs/api_reference/STL/stldbstl_global_functionslock_mutex.html
---
## lock_mutex

### Function Details

``` c
 int lock_mutex(db_mutex_t mtx)
 
```

Lock a mutex, wait if it is held by another thread.

#### Parameters

##### mtx

The mutex handle to lock.

#### Return Value

0 if succeed, non-zero otherwise, call db_strerror to get message.

### Group: Mutex API based on Berkeley DB mutex.

These functions are in-process mutex support which uses Berkeley DB mutex mechanisms.

You can call these functions to do portable synchronization for your code.

### Class

<a href="dbstl_global_functions.md" class="link" title="Chapter 1.  Dbstl Global Public Functions">dbstl_global_functions</a>
