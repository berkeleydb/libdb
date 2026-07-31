---
title: "db_multimap"
api-name: "db_multimap"
source: docs/api_reference/STL/stldb_multimapdb_multimap.html
---
## db_multimap

### Function Details

``` c
db_multimap(Db *dbp=NULL,
    DbEnv *envp=NULL)
 
```

Constructor.

See class detail for handle requirement.

#### Parameters

##### dbp

The database handle.

##### envp

The database environment handle.

#### See Also

<a href="db_map.md#stldb_mapdb_map" class="link" title="db_map">db_map::db_map(Db*, DbEnv*)</a> <a href="stldb_vectordb_vector.md" class="link" title="db_vector">db_vector::db_vector(Db*, DbEnv*)</a>

``` c
db_multimap(Db *dbp, DbEnv *envp, InputIterator first,
    InputIterator last)
 
```

Iteration constructor.

Iterates between first and last, setting a copy of each of the sequence of elements as the content of the container object. This function supports auto-commit. See class detail for handle requirement.

#### Parameters

##### dbp

The database handle.

##### envp

The database environment handle.

##### last

The open boundary of the range.

##### first

The closed boundary of the range.

#### See Also

<a href="db_map.md#stldb_mapdb_map" class="link" title="db_map">db_map::db_map(Db*, DbEnv*, InputIterator, InputIterator)</a> <a href="stldb_vectordb_vector.md" class="link" title="db_vector">db_vector::db_vector(Db*, DbEnv*)</a>

``` c
db_multimap(const self &x)
 
```

Copy constructor.

Create an database and insert all key/data pairs in x into this container. x's data members are not copied. This function supports auto-commit.

#### Parameters

##### x

The other container to initialize this container.

#### See Also

<a href="stldb_containerdb_container.md" class="link" title="db_container">db_container(const db_container&amp;)</a> <a href="db_map.md#stldb_mapdb_map" class="link" title="db_map">db_map(const db_map&amp;)</a>

### Class

<a href="db_multimap.md" class="link" title="Chapter 6.  Db_multimap">db_multimap</a>
