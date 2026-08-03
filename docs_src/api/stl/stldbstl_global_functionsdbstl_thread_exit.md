---
title: "dbstl_thread_exit"
api-name: "dbstl_thread_exit"
source: docs/api_reference/STL/stldbstl_global_functionsdbstl_thread_exit.html
---
## dbstl_thread_exit

### Function Details

``` c
 void dbstl_thread_exit()
 
```

This function closes all Berkeley DB handles in the right order, if other threads do not use them.

You can call this function before a thread exits to close unused Berkeley DB handles.

### Class

<a href="dbstl_global_functions.md" class="link" title="Chapter 1.  Dbstl Global Public Functions">dbstl_global_functions</a>
