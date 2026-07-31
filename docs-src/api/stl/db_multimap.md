---
title: "Chapter 6.  Db_multimap"
api-name: "Chapter 6.  Db_multimap"
source: docs/api_reference/STL/db_multimap.html
---
## Chapter 6.  Db_multimap

This class is the combination of std::multimap and hash_multimap.

By setting database handles as DB_BTREE or DB_HASH type respectively, you will be using an equivalent of std::multimap or hash_multimap respectively. Database(dbp) and environment(penv) handle requirement: The dbp handle must meet the following requirement: 1. Database type should be DB_BTREE or DB_HASH. 2. Either DB_DUP or DB_DUPSORT flag must be set. Note that so far Berkeley DB does not allow DB_DUPSORT be set and the database is storing identical key/data pairs, i.e. we can't store two (1, 2), (1, 2) pairs into a database D with DB_DUPSORT flag set, but only can do so with DB_DUP flag set; But we can store a (1, 2) pair and a (1, 3) pair into D with DB_DUPSORT flag set. So if your data set allows DB_DUPSORT flag, you should set it to gain a lot of performance promotion. 3. No DB_RECNUM flag set. 4. No DB_TRUNCATE specified in database open flags. 5. DB_THREAD must be set if you are sharing the database handle across multiple threads directly, or indirectly by sharing the container object across multiple threads.

#### See Also

<a href="db_container.md" class="link" title="Chapter 3.  Db_container">db_container</a> <a href="db_map.md" class="link" title="Chapter 5.  Db_map">db_map</a>

### Class Template Parameters

#### kdt

The key data type.

#### ddt

The data data type. <a href="db_multimap.md" class="link" title="Chapter 6.  Db_multimap">db_multimap</a> stores key/data pairs.

#### value_type_sub

Do not specify anything if ddt type is a class/struct type; Otherwise, specify ElementHolder\<ddt\> to it.

#### iterator_t

Never specify anything to this type parameter. It is only used internally.

#### Public Members

| Member | Description |
|----|----|
| <a href="db_multimap.md#stldb_multimapinsert" class="xref" title="insert">insert</a> | Range insertion. |
| <a href="stldb_multimaperase.md" class="xref" title="erase">erase</a> | Erase elements by key. |
| <a href="stldb_multimapequal_range.md" class="xref" title="equal_range">equal_range</a> | Find the range within which all keys equal to specified key x. |
| <a href="stldb_multimapequal_range_N.md" class="xref" title="equal_range_N">equal_range_N</a> | Find equal range and number of key/data pairs in the range. |
| <a href="stldb_multimapcount.md" class="xref" title="count">count</a> | Count the number of key/data pairs having specified key x. |
| <a href="stldb_multimapupper_bound.md" class="xref" title="upper_bound">upper_bound</a> | Find the least key greater than x. |
| <a href="stldb_multimapdb_multimap.md" class="xref" title="db_multimap">db_multimap</a> | Constructor. |
| <a href="stldb_multimapdstr_db_multimap.md" class="xref" title="~db_multimap">~db_multimap</a> |  |
| <a href="stldb_multimapoperator_assign.md" class="xref" title="operator=">operator=</a> | Container content assignment operator. |
| <a href="stldb_multimapswap.md" class="xref" title="swap">swap</a> | Swap content with another multimap container. |
| <a href="stldb_multimapoperator_eq.md" class="xref" title="operator==">operator==</a> | Returns whether the two containers have identical content. |
| <a href="stldb_multimapoperator_ueq.md" class="xref" title="operator!=">operator!=</a> | Container unequality comparison operator. |

#### Group

<a href="dbstl_containers.md" class="xref" title="Chapter 2.  Dbstl Container Classes">Dbstl Container Classes</a>

## insert

### Function Details

``` c
void insert(InputIterator first,
    InputIterator last)
 
```

Range insertion.

Insert a range \[first, last) of key/data pairs into this container.

#### Parameters

##### last

The open boundary of the range.

##### first

The closed boundary of the range.

``` c
void insert(const_iterator &first,
    const_iterator &last)
 
```

Range insertion.

Insert a range \[first, last) of key/data pairs into this container.

#### Parameters

##### last

The open boundary of the range.

##### first

The closed boundary of the range.

``` c
iterator insert(const value_type &x)
 
```

Insert a single key/data pair if the key is not in the container.

#### Parameters

##### x

The key/data pair to insert.

#### Return Value

A pair P, if insert OK, i.e. the inserted key wasn't in the container, P.first will be the iterator sitting on the inserted key/data pair, and P.second is true; otherwise P.first is an invalid iterator and P.second is false.

### Group: Insert Functions

<a href="http://www.cplusplus.com/reference/stl/multimap/insert/" class="ulink" target="_top">http://www.cplusplus.com/reference/stl/multimap/insert/</a>

### Class

<a href="db_multimap.md" class="link" title="Chapter 6.  Db_multimap">db_multimap</a>
