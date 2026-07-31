---
title: "free_mutex"
api-name: "free_mutex"
source: docs/api_reference/STL/stldbstl_global_functionsfree_mutex.html
---
## free_mutex

### Function Details

``` c
 void free_mutex(db_mutex_t mtx)
 
```

Free a mutex, and return immediately.

#### Parameters

##### mtx

The mutex handle to free.

#### Return Value

0 if succeed, non-zero otherwise, call db_strerror to get message.

### Group: Mutex API based on Berkeley DB mutex.

These functions are in-process mutex support which uses Berkeley DB mutex mechanisms.

You can call these functions to do portable synchronization for your code.

### Class

<a href="dbstl_global_functions.md" class="link" title="Chapter 1.  Dbstl Global Public Functions">dbstl_global_functions</a>
