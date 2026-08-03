---
title: "~db_container"
api-name: "~db_container"
source: docs/api_reference/STL/stldb_containerdstr_db_container.html
---
## ~db_container

### Function Details

``` c
virtual ~db_container()
 
```

The backing database is not closed in this function.

It is closed when current thread exits and the database is no longer referenced by any other container instances in this process. In order to make the reference counting work alright, you must call <a href="stldbstl_global_functionsregister_db.md" class="link" title="register_db">register_db(Db*)</a> and <a href="stldbstl_global_functionsregister_db_env.md" class="link" title="register_db_env">register_db_env(DbEnv*)</a> correctly.

#### See Also

<a href="stldbstl_global_functionsregister_db.md" class="link" title="register_db">register_db(Db*)</a> <a href="stldbstl_global_functionsregister_db_env.md" class="link" title="register_db_env">register_db_env(DbEnv*)</a>

### Class

<a href="db_container.md" class="link" title="Chapter 3.  Db_container">db_container</a>
