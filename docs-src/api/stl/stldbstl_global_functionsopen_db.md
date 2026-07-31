---
title: "open_db"
api-name: "open_db"
source: docs/api_reference/STL/stldbstl_global_functionsopen_db.html
---
## open_db

### Function Details

``` c
 Db* open_db(DbEnv *penv, const char *filename, DBTYPE dbtype,
    u_int32_t oflags, u_int32_t set_flags, int mode=0644, DbTxn *txn=NULL,
    u_int32_t cflags=0,
    const char *dbname=NULL)
 
```

Helper function to open a database and register it into dbstl for the calling thread.

Users still need to register it in any other thread using it if it is shared by multiple threads, via <a href="stldbstl_global_functionsregister_db.md" class="link" title="register_db">register_db()</a> function. Users don't need to delete or free the memory of the returned object, dbstl will take care of that. When you don't use <a href="stldbstl_global_functionsopen_db.md" class="link" title="open_db">dbstl::open_db()</a> but explicitly call DB C++ API to open a database, you must new the Db object, rather than create it on stack, and you must delete the Db object by yourself.

#### Parameters

##### penv

The environment to open the database from.

##### txn

The transaction to open the database from, passed to Db::open.

##### dbtype

The database type, passed to Db::open.

##### oflags

The database open flags, passed to Db::open.

##### filename

The database file name, passed to Db::open.

##### mode

The database open mode, passed to Db::open.

##### cflags

The create flags passed to Db class constructor.

##### dbname

The database name, passed to Db::open.

##### set_flags

The flags to be set to the created database handle.

#### Return Value

The opened database handle.

#### See Also

<a href="stldbstl_global_functionsregister_db.md" class="link" title="register_db">register_db(Db *)</a> ;

open_db_env;

### Class

<a href="dbstl_global_functions.md" class="link" title="Chapter 1.  Dbstl Global Public Functions">dbstl_global_functions</a>
