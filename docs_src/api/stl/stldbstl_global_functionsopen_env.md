---
title: "open_env"
api-name: "open_env"
source: docs/api_reference/STL/stldbstl_global_functionsopen_env.html
---
## open_env

### Function Details

``` c
 DbEnv* open_env(const char *env_home, u_int32_t set_flags,
    u_int32_t oflags=DB_CREATE|DB_INIT_MPOOL, 
    u_int32_t cachesize=4 *1024 *1024, int mode=0644,
    u_int32_t cflags=0)
 
```

Helper function to open an environment and register it into dbstl for the calling thread.

Users still need to register it in any other thread if it is shared by multiple threads, via <a href="stldbstl_global_functionsregister_db_env.md" class="link" title="register_db_env">register_db_env()</a> function above. Users don't need to delete or free the memory of the returned object, dbstl will take care of that.

When you don't use <a href="stldbstl_global_functionsopen_env.md" class="link" title="open_env">dbstl::open_env()</a> but explicitly call DB C++ API to open an environment, you must new the DbEnv object, rather than create it on stack, and you must delete the DbEnv object by yourself.

#### Parameters

##### oflags

Environment open flags, passed to DbEnv::open.

##### set_flags

Flags to set to the created environment before opening it.

##### mode

Environment region files mode, passed to DbEnv::open.

##### cflags

DbEnv constructor creation flags, passed to DbEnv::DbEnv.

##### cachesize

Environment cache size, by default 4M bytes.

##### env_home

Environment home directory, it must exist. Passed to DbEnv::open.

#### Return Value

The opened database environment handle.

#### See Also

<a href="stldbstl_global_functionsregister_db_env.md" class="link" title="register_db_env">register_db_env(DbEnv *)</a> ;

<a href="stldbstl_global_functionsopen_db.md" class="link" title="open_db">open_db</a> ;

### Class

<a href="dbstl_global_functions.md" class="link" title="Chapter 1.  Dbstl Global Public Functions">dbstl_global_functions</a>
