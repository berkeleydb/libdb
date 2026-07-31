---
title: "dbstl_startup"
api-name: "dbstl_startup"
source: docs/api_reference/STL/stldbstl_global_functionsdbstl_startup.html
---
## dbstl_startup

### Function Details

``` c
 void dbstl_startup()
 
```

If there are multiple threads within a process that make use of dbstl, then this function should be called in a single thread mutual exclusively before any use of dbstl in a process; Otherwise, you don't need to call it, but are allowed to call it anyway.

### Class

<a href="dbstl_global_functions.md" class="link" title="Chapter 1.  Dbstl Global Public Functions">dbstl_global_functions</a>
