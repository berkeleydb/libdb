---
title: "Chapter 7.  Db_set"
api-name: "Chapter 7.  Db_set"
source: docs/api_reference/STL/db_set.html
---
## Chapter 7.  Db_set

This class is the combination of std::set and hash_set.

By setting database handles of DB_BTREE or DB_HASH type, you will be using the equivalent of std::set or hash_set. This container stores the key in the key element of a key/data pair in the underlying database, but doesn't store anything in the data element. Database and environment handle requirement: The same as that of <a href="db_map.md" class="link" title="Chapter 5.  Db_map">db_map</a> .

#### See Also

<a href="db_map.md" class="link" title="Chapter 5.  Db_map">db_map</a> <a href="db_container.md" class="link" title="Chapter 3.  Db_container">db_container</a>

### Class Template Parameters

#### kdt

The key data type.

#### value_type_sub

If kdt is a class/struct type, do not specify anything in this parameter; Otherwise specify ElementHolder\<kdt\>.

#### Public Members

| Member | Description |
|----|----|
| <a href="db_set.md#stldb_setdb_set" class="xref" title="db_set">db_set</a> | Create a std::set/hash_set equivalent associative container. |
| <a href="stldb_setdstr_db_set.md" class="xref" title="~db_set">~db_set</a> |  |
| <a href="stldb_setinsert.md" class="xref" title="insert">insert</a> | Insert a single key/data pair if the key is not in the container. |
| <a href="stldb_setoperator_assign.md" class="xref" title="operator=">operator=</a> | Container content assignment operator. |
| <a href="stldb_setvalue_comp.md" class="xref" title="value_comp">value_comp</a> | Get value comparison functor. |
| <a href="stldb_setswap.md" class="xref" title="swap">swap</a> | Swap content with another container. |
| <a href="stldb_setoperator_eq.md" class="xref" title="operator==">operator==</a> | Set content equality comparison operator. |
| <a href="stldb_setoperator_ueq.md" class="xref" title="operator!=">operator!=</a> | Inequality comparison operator. |

#### Group

<a href="dbstl_containers.md" class="xref" title="Chapter 2.  Dbstl Container Classes">Dbstl Container Classes</a>

## db_set

### Function Details

``` c
db_set(Db *dbp=NULL,
    DbEnv *envp=NULL)
 
```

Create a std::set/hash_set equivalent associative container.

See the handle requirement in class details to pass correct database/environment handles.

#### Parameters

##### dbp

The database handle.

##### envp

The database environment handle.

#### See Also

<a href="db_map.md#stldb_mapdb_map" class="link" title="db_map">db_map(Db*, DbEnv*)</a> <a href="stldb_containerdb_container.md" class="link" title="db_container">db_container(Db*, DbEnv*)</a>

``` c
db_set(Db *dbp, DbEnv *envp, InputIterator first,
    InputIterator last)
 
```

Iteration constructor.

Iterates between first and last, copying each of the elements in the range into this container. Create a std::set/hash_set equivalent associative container. Insert a range of elements into the database. The range is \[first, last), which contains elements that can be converted to type ddt automatically. This function supports auto-commit. See the handle requirement in class details to pass correct database/environment handles.

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

<a href="db_map.md#stldb_mapdb_map" class="link" title="db_map">db_map(Db*, DbEnv*, InputIterator, InputIterator)</a>

``` c
db_set(const self &x)
 
```

Copy constructor.

Create a database and insert all key/data pairs in x into this container. x's data members are not copied. This function supports auto-commit.

#### Parameters

##### x

The source container to initialize this container.

#### See Also

<a href="db_map.md#stldb_mapdb_map" class="link" title="db_map">db_map(const db_map&amp;)</a> <a href="stldb_containerdb_container.md" class="link" title="db_container">db_container(const db_container&amp;)</a>

### Class

<a href="db_set.md" class="link" title="Chapter 7.  Db_set">db_set</a>
