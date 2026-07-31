---
title: "dbstl_exit"
api-name: "dbstl_exit"
source: docs/api_reference/STL/stldbstl_global_functionsdbstl_exit.html
---
## dbstl_exit

### Function Details

``` c
 void dbstl_exit()
 
```

This function releases memory allocated by dbstl on the heap, and closes all Berkeley DB handles in the right order.

You can call <a href="stldbstl_global_functionsdbstl_exit.md" class="link" title="dbstl_exit">dbstl_exit()</a> before the process exits to release any memory allocated by dbstl that has to persist during the entire process lifetime.

### Class

<a href="dbstl_global_functions.md" class="link" title="Chapter 1.  Dbstl Global Public Functions">dbstl_global_functions</a>
