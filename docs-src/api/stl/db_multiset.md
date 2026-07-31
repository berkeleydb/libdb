---
title: "Chapter 8.  Db_multiset"
api-name: "Chapter 8.  Db_multiset"
source: docs/api_reference/STL/db_multiset.html
---
## Chapter 8.  Db_multiset

This class is the combination of std::multiset and hash_multiset.

By setting database handles of DB_BTREE or DB_HASH type respectively, you will be using the equivalent of std::multiset or hash_multiset respectively. This container stores the key in the key element of a key/data pair in the underlying database, but doesn't store anything in the data element. Database and environment handle requirement: The requirement to these handles is the same as that to <a href="db_multimap.md" class="link" title="Chapter 6.  Db_multimap">db_multimap</a> .

#### See Also

<a href="db_multimap.md" class="link" title="Chapter 6.  Db_multimap">db_multimap</a> <a href="db_map.md" class="link" title="Chapter 5.  Db_map">db_map</a> <a href="db_container.md" class="link" title="Chapter 3.  Db_container">db_container</a> <a href="db_set.md" class="link" title="Chapter 7.  Db_set">db_set</a>

### Class Template Parameters

#### kdt

The key data type.

#### value_type_sub

If kdt is a class/struct type, do not specify anything in this parameter; Otherwise specify ElementHolder\<kdt\>.

#### Public Members

| Member | Description |
|----|----|
| <a href="db_multiset.md#stldb_multisetdb_multiset" class="xref" title="db_multiset">db_multiset</a> | Create a std::multiset/hash_multiset equivalent associative container. |
| <a href="stldb_multisetdstr_db_multiset.md" class="xref" title="~db_multiset">~db_multiset</a> |  |
| <a href="stldb_multisetinsert.md" class="xref" title="insert">insert</a> | Insert a single key if the key is not in the container. |
| <a href="stldb_multiseterase.md" class="xref" title="erase">erase</a> | Erase elements by key. |
| <a href="stldb_multisetoperator_assign.md" class="xref" title="operator=">operator=</a> | Container content assignment operator. |
| <a href="stldb_multisetswap.md" class="xref" title="swap">swap</a> | Swap content with another container. |
| <a href="stldb_multisetoperator_eq.md" class="xref" title="operator==">operator==</a> | Container content equality compare operator. |
| <a href="stldb_multisetoperator_ueq.md" class="xref" title="operator!=">operator!=</a> | Inequality comparison operator. |

#### Group

<a href="dbstl_containers.md" class="xref" title="Chapter 2.  Dbstl Container Classes">Dbstl Container Classes</a>

## db_multiset

### Function Details

``` c
db_multiset(Db *dbp=NULL,
    DbEnv *envp=NULL)
 
```

Create a std::multiset/hash_multiset equivalent associative container.

See the handle requirement in class details to pass correct database/environment handles.

#### Parameters

##### dbp

The database handle.

##### envp

The database environment handle.

#### See Also

<a href="stldb_multimapdb_multimap.md" class="link" title="db_multimap">db_multimap(Db*, DbEnv*)</a>

``` c
db_multiset(Db *dbp, DbEnv *envp, InputIterator first,
    InputIterator last)
 
```

Iteration constructor.

Iterates between first and last, copying each of the elements in the range into this container. Create a std::multi/hash_multiset equivalent associative container. Insert a range of elements into the database. The range is \[first, last), which contains elements that can be converted to type ddt automatically. This function supports auto-commit. See the handle requirement in class details to pass correct database/environment handles.

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

<a href="stldb_multimapdb_multimap.md" class="link" title="db_multimap">db_multimap(Db*, DbEnv*, InputIterator, InputIterator)</a>

``` c
db_multiset(const self &x)
 
```

Copy constructor.

Create a database and insert all key/data pairs in x into this container. x's data members are not copied. This function supports auto-commit.

#### Parameters

##### x

The source container to initialize this container.

#### See Also

db_multimap(const db_multimap&) <a href="stldb_containerdb_container.md" class="link" title="db_container">db_container(const db_container&amp;)</a>

### Class

<a href="db_multiset.md" class="link" title="Chapter 8.  Db_multiset">db_multiset</a>
