---
title: "Chapter 5.  Db_map"
api-name: "Chapter 5.  Db_map"
source: docs/api_reference/STL/db_map.html
---
## Chapter 5.  Db_map

<a href="db_map.md" class="link" title="Chapter 5.  Db_map">db_map</a> has identical methods to std::map and the semantics for each method is identical to its std::map counterpart, except that it stores data into underlying Berkeley DB btree or hash database.

Passing a database handle of btree or hash type creates a <a href="db_map.md" class="link" title="Chapter 5.  Db_map">db_map</a> equivalent to std::map and std::hashmap respectively. Database(dbp) and environment(penv) handle requirement(applies to all constructors in this class template): 0. The dbp is opened inside the penv environment. Either one of the two handles can be NULL. If dbp is NULL, an anonymous database is created by dbstl. 1. Database type of dbp should be DB_BTREE or DB_HASH. 2. No DB_DUP or DB_DUPSORT flag set in dbp. 3. No DB_RECNUM flag set in dbp. 4. No DB_TRUNCATE specified in dbp's database open flags. 5. DB_THREAD must be set if you are sharing the dbp across multiple threads directly, or indirectly by sharing the container object across multiple threads.

#### See Also

<a href="db_container.md" class="link" title="Chapter 3.  Db_container">db_container</a> <a href="stldb_containerdb_container.md" class="link" title="db_container">db_container(Db*, DbEnv*)</a> <a href="stldb_containerdb_container.md" class="link" title="db_container">db_container(const db_container&amp;)</a>

### Class Template Parameters

#### kdt

The key data type.

#### ddt

The data data type. <a href="db_map.md" class="link" title="Chapter 5.  Db_map">db_map</a> stores key/data pairs.

#### value_type_sub

Do not specify anything if ddt type is a class/struct type; Otherwise, specify ElementHolder\<ddt\> to it.

#### iterator_t

Never specify anything to this type parameter. It is only used internally.

#### Public Members

| Member | Description |
|----|----|
| <a href="db_map.md#stldb_mapdb_map" class="xref" title="db_map">db_map</a> | Create a std::map/hash_map equivalent associative container. |
| <a href="stldb_mapdstr_db_map.md" class="xref" title="~db_map">~db_map</a> |  |
| <a href="stldb_mapinsert.md" class="xref" title="insert">insert</a> | Insert a single key/data pair if the key is not in the container. |
| <a href="stldb_mapbegin.md" class="xref" title="begin">begin</a> | Begin a read-write or readonly iterator which sits on the first key/data pair of the database. |
| <a href="stldb_mapend.md" class="xref" title="end">end</a> | Create an open boundary iterator. |
| <a href="stldb_maprbegin.md" class="xref" title="rbegin">rbegin</a> | Begin a read-write or readonly reverse iterator which sits on the first key/data pair of the database. |
| <a href="stldb_maprend.md" class="xref" title="rend">rend</a> | Create an open boundary iterator. |
| <a href="stldb_mapis_hash.md" class="xref" title="is_hash">is_hash</a> | Get container category. |
| <a href="stldb_mapbucket_count.md" class="xref" title="bucket_count">bucket_count</a> | Only for std::hash_map, return number of hash bucket in use. |
| <a href="stldb_mapsize.md" class="xref" title="size">size</a> | This function supports auto-commit. |
| <a href="stldb_mapmax_size.md" class="xref" title="max_size">max_size</a> | Get max size. |
| <a href="stldb_mapempty.md" class="xref" title="empty">empty</a> | Returns whether this container is empty. |
| <a href="stldb_maperase.md" class="xref" title="erase">erase</a> | Erase a key/data pair at specified position. |
| <a href="stldb_mapfind.md" class="xref" title="find">find</a> | Find the key/data pair with specified key x. |
| <a href="stldb_maplower_bound.md" class="xref" title="lower_bound">lower_bound</a> | Find the greatest key less than or equal to x. |
| <a href="stldb_mapequal_range.md" class="xref" title="equal_range">equal_range</a> | Find the range within which all keys equal to specified key x. |
| <a href="stldb_mapcount.md" class="xref" title="count">count</a> | Count the number of key/data pairs having specified key x. |
| <a href="stldb_mapupper_bound.md" class="xref" title="upper_bound">upper_bound</a> | Find the least key greater than x. |
| <a href="stldb_mapkey_eq.md" class="xref" title="key_eq">key_eq</a> | Function to get key compare functor. |
| <a href="stldb_maphash_funct.md" class="xref" title="hash_funct">hash_funct</a> | Function to get hash key generating functor. |
| <a href="stldb_mapvalue_comp.md" class="xref" title="value_comp">value_comp</a> | Function to get value compare functor. |
| <a href="stldb_mapkey_comp.md" class="xref" title="key_comp">key_comp</a> | Function to get key compare functor. |
| <a href="stldb_mapoperator_assign.md" class="xref" title="operator=">operator=</a> | Container content assignment operator. |
| <a href="stldb_mapoperator_sqbrk.md" class="xref" title="operator[]">operator[]</a> | Retrieve data element by key. |
| <a href="stldb_mapswap.md" class="xref" title="swap">swap</a> | Swap content with container mp. |
| <a href="stldb_mapclear.md" class="xref" title="clear">clear</a> | Clear contents in this container. |
| <a href="stldb_mapoperator_eq.md" class="xref" title="operator==">operator==</a> | Map content equality comparison operator. |
| <a href="stldb_mapoperator_ueq.md" class="xref" title="operator!=">operator!=</a> | Container unequality comparison operator. |

#### Group

<a href="dbstl_containers.md" class="xref" title="Chapter 2.  Dbstl Container Classes">Dbstl Container Classes</a>

## db_map

### Function Details

``` c
db_map(Db *dbp=NULL,
    DbEnv *envp=NULL)
 
```

Create a std::map/hash_map equivalent associative container.

See the handle requirement in class details to pass correct database/environment handles.

#### Parameters

##### dbp

The database handle.

##### envp

The database environment handle.

#### See Also

<a href="stldb_containerdb_container.md" class="link" title="db_container">db_container(Db*, DbEnv*)</a>

``` c
db_map(Db *dbp, DbEnv *envp, InputIterator first,
    InputIterator last)
 
```

Iteration constructor.

Iterates between first and last, setting a copy of each of the sequence of elements as the content of the container object. Create a std::map/hash_map equivalent associative container. Insert a range of elements into the database. The range is \[first, last), which contains elements that can be converted to type ddt automatically. See the handle requirement in class details to pass correct database/environment handles. This function supports auto-commit.

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

<a href="stldb_containerdb_container.md" class="link" title="db_container">db_container(Db*, DbEnv*)</a>

``` c
db_map(const db_map< kdt, ddt, value_type_sub,
    iterator > &x)
 
```

Copy constructor.

Create an database and insert all key/data pairs in x into this container. x's data members are not copied. This function supports auto-commit.

#### Parameters

##### x

The other container to initialize this container.

#### See Also

<a href="stldb_containerdb_container.md" class="link" title="db_container">db_container(const db_container&amp;)</a>

### Class

<a href="db_map.md" class="link" title="Chapter 5.  Db_map">db_map</a>
