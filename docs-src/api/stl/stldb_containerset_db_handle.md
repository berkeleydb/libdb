---
title: "set_db_handle"
api-name: "set_db_handle"
source: docs/api_reference/STL/stldb_containerset_db_handle.html
---
## set_db_handle

### Function Details

``` c
void set_db_handle(Db *dbp,
    DbEnv *newenv=NULL)
 
```

Set the underlying database's handle, and optionally environment handle if the environment has also changed.

That is, users can change the container object's underlying database while the object is alive. dbstl will verify that the handles set conforms to the concrete container's requirement to Berkeley DB database/environment handles.

#### Parameters

##### dbp

The database handle to set.

##### newenv

The database environment handle to set.

### Group: Get and set functions for data members.

Note that these functions are not thread safe, because all data members of <a href="db_container.md" class="link" title="Chapter 3.  Db_container">db_container</a> are supposed to be set on container construction and initialization, and remain read only afterwards.

### Class

<a href="db_container.md" class="link" title="Chapter 3.  Db_container">db_container</a>
