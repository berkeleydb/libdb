---
title: "db_vector"
api-name: "db_vector"
source: docs/api_reference/STL/stldb_vectordb_vector.html
---
## db_vector

### Function Details

``` c
db_vector(Db *dbp=NULL,
    DbEnv *penv=NULL)
 
```

Constructor.

Note that we do not need an allocator in db-stl containser, but we need backing up Db\* and DbEnv\*, and we have to verify that the passed in bdb handles are valid for use by the container class. See class detail for handle requirement.

#### Parameters

##### dbp

The same as that of <a href="stldb_containerdb_container.md" class="link" title="db_container">db_container(Db*, DbEnv*)</a> ;

##### penv

The same as that of <a href="stldb_containerdb_container.md" class="link" title="db_container">db_container(Db*, DbEnv*)</a> ;

#### See Also

<a href="stldb_containerdb_container.md" class="link" title="db_container">db_container(Db*, DbEnv*)</a> ;

``` c
db_vector(size_type n, const T &val=T(), Db *dbp=NULL,
    DbEnv *penv=NULL)
 
```

Constructor.

This function supports auto-commit. Insert n elements of T type into the database, the value of the elements is the default value or user set value. See class detail for handle requirement.

#### Parameters

##### dbp

The same as that of <a href="stldb_containerdb_container.md" class="link" title="db_container">db_container(Db*, DbEnv*)</a> ;

##### penv

The same as that of <a href="stldb_containerdb_container.md" class="link" title="db_container">db_container(Db*, DbEnv*)</a> ;

##### val

The value of elements to insert.

##### n

The number of elements to insert.

#### See Also

<a href="stldb_vectordb_vector.md" class="link" title="db_vector">db_vector(Db*, DbEnv*)</a> ; <a href="stldb_containerdb_container.md" class="link" title="db_container">db_container(Db*, DbEnv*)</a> ;

``` c
db_vector(const self &x)
 
```

Copy constructor.

This function supports auto-commit. Insert all elements in x into this container.

#### See Also

<a href="stldb_containerdb_container.md" class="link" title="db_container">db_container(const db_container&amp;)</a>

``` c
db_vector(Db *dbp, DbEnv *penv, InputIterator first,
    InputIterator last)
 
```

Insert a range of elements into this container.

The range is \[first, last), which contains elements that can be converted to type T automatically. See class detail for handle requirement.

#### Parameters

##### dbp

The same as that of <a href="stldb_containerdb_container.md" class="link" title="db_container">db_container(Db*, DbEnv*)</a> ;

##### first

Range closed boundary.

##### last

Range open boundary.

##### penv

The same as that of <a href="stldb_containerdb_container.md" class="link" title="db_container">db_container(Db*, DbEnv*)</a> ;

#### See Also

<a href="stldb_vectordb_vector.md" class="link" title="db_vector">db_vector(Db*, DbEnv*)</a> ;

``` c
db_vector(const_iterator first, const_iterator last, Db *dbp=NULL,
    DbEnv *penv=NULL)
 
```

Range constructor.

This function supports auto-commit. Insert the range of elements in \[first, last) into this container. See class detail for handle requirement.

#### Parameters

##### dbp

The same as that of <a href="stldb_containerdb_container.md" class="link" title="db_container">db_container(Db*, DbEnv*)</a> ;

##### first

Range closed boundary.

##### last

Range open boundary.

##### penv

The same as that of <a href="stldb_containerdb_container.md" class="link" title="db_container">db_container(Db*, DbEnv*)</a> ;

#### See Also

<a href="stldb_vectordb_vector.md" class="link" title="db_vector">db_vector(Db*, DbEnv*)</a> ;

### Class

<a href="db_vector.md" class="link" title="Chapter 4.  Db_vector">db_vector</a>
