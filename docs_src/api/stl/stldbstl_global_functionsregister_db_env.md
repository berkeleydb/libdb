---
title: "register_db_env"
api-name: "register_db_env"
source: docs/api_reference/STL/stldbstl_global_functionsregister_db_env.html
---
## register_db_env

### Function Details

``` c
 void register_db_env(DbEnv *env1)
 
```

Register a DbEnv handle env1, this handle and handles opened in it will be closed by ResourceManager .

Application code must not try to close or delete it. Users can do enough config before opening the DbEnv and then register it via this function. All environment handles should be registered via this function in each thread using the handle. The only exception is the environment handle opened by dbstl::open_db_env should not be registered in the thread of the dbstl::open_db_env call.

#### Parameters

##### env1

The environment to register into dbstl for current thread.

### Class

<a href="dbstl_global_functions.md" class="link" title="Chapter 1.  Dbstl Global Public Functions">dbstl_global_functions</a>
