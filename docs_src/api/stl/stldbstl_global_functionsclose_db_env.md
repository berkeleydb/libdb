---
title: "close_db_env"
api-name: "close_db_env"
source: docs/api_reference/STL/stldbstl_global_functionsclose_db_env.html
---
## close_db_env

### Function Details

``` c
 void close_db_env(DbEnv *pdbenv)
 
```

Close specified database environment handle regardless of reference count.

Make sure the environment is not used by any other databases.

#### Parameters

##### pdbenv

The database environment handle to close.

### Group: Functions to close database/environments.

Normally you don't have to close any database or environment handles, they will be closed automatically.

Though you still have the following API to close them.

### Class

<a href="dbstl_global_functions.md" class="link" title="Chapter 1.  Dbstl Global Public Functions">dbstl_global_functions</a>
