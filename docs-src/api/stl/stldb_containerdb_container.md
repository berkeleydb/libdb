---
title: "db_container"
api-name: "db_container"
source: docs/api_reference/STL/stldb_containerdb_container.html
---
## db_container

### Function Details

``` c
db_container()
 
```

Default constructor.

``` c
db_container(const db_container &dbctnr)
 
```

Copy constructor.

The new container will be backed by another database within the same environment unless dbctnr's backing database is in its own internal private environment. The name of the database is coined based on current time and thread id and some random number. If this is still causing naming clashes, you can set a suffix number via "set_global_dbfile_suffix_number" function; And following db file will suffix this number in the file name for additional randomness. And the suffix will be incremented after each such use. You can change the file name via DbEnv::rename. If dbctnr is using an anonymous database, the newly constructed container will also use an anonymous one.

#### Parameters

##### dbctnr

The container to initialize this container.

``` c
db_container(Db *dbp,
    DbEnv *envp)
 
```

This constructor is not directly called by the user, but invoked by constructors of concrete container classes.

The statement about the parameters applies to constructors of all container classes.

#### Parameters

##### dbp

Database handle. dbp is supposed to be opened inside envp. Each dbstl container is backed by a Berkeley DB database, so dbstl will create an internal anonymous database if dbp is NULL.

##### envp

Environment handle. And envp can also be NULL, meaning the dbp handle may be created in its internal private environment.

### Class

<a href="db_container.md" class="link" title="Chapter 3.  Db_container">db_container</a>
