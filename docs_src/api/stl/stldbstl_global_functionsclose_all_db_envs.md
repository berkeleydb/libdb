---
title: "close_all_db_envs"
api-name: "close_all_db_envs"
source: docs/api_reference/STL/stldbstl_global_functionsclose_all_db_envs.html
---
## close_all_db_envs

### Function Details

``` c
 void close_all_db_envs()
 
```

Close all open database environment handles regardless of reference count.

You can't use the container after you called close_db and before setting another valid database handle to the container via <a href="stldb_containerset_db_handle.md" class="link" title="set_db_handle">db_container::set_db_handle()</a> function.

#### See Also

<a href="stldbstl_global_functionsclose_db_env.md" class="link" title="close_db_env">close_db_env(DbEnv *)</a> ;

### Group: Functions to close database/environments.

Normally you don't have to close any database or environment handles, they will be closed automatically.

Though you still have the following API to close them.

### Class

<a href="dbstl_global_functions.md" class="link" title="Chapter 1.  Dbstl Global Public Functions">dbstl_global_functions</a>
