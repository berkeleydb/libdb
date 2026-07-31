---
title: "Chapter 16.  Db_map_iterator"
api-name: "Chapter 16.  Db_map_iterator"
source: docs/api_reference/STL/db_map_iterator.html
---
## Chapter 16.  Db_map_iterator

#### Public Members

| Member | Description |
|----|----|
| <a href="db_map_iterator.md#stldb_map_iteratordb_map_iterator" class="xref" title="db_map_iterator">db_map_iterator</a> | Copy constructor. |
| <a href="stldb_map_iteratordstr_db_map_iterator.md" class="xref" title="~db_map_iterator">~db_map_iterator</a> | Destructor. |
| <a href="stldb_map_iteratoroperator_incr.md" class="xref" title="operator++">operator++</a> | Pre-increment. |
| <a href="stldb_map_iteratoroperator_decr.md" class="xref" title="operator--">operator--</a> | Pre-decrement. |
| <a href="stldb_map_iteratoroperator__star.md" class="xref" title="operator *">operator *</a> | Dereference operator. |
| <a href="stldb_map_iteratoroperator_arrow.md" class="xref" title="operator-&gt;">operator-&gt;</a> | Arrow operator. |
| <a href="stldb_map_iteratorrefresh.md" class="xref" title="refresh">refresh</a> | Refresh iterator cached value. |
| <a href="stldb_map_iteratoroperator_assign.md" class="xref" title="operator=">operator=</a> | Assignment operator. |

#### Group

<a href="dbstl_iterators.md" class="xref" title="Chapter 9.  Dbstl Iterator Classes">Dbstl Iterator Classes</a>

## db_map_iterator

### Function Details

``` c
db_map_iterator(const db_map_iterator< kdt, ddt,
    value_type_sub > &vi)
 
```

Copy constructor.

#### Parameters

##### vi

The other iterator of the same type to initialize this.

``` c
db_map_iterator(const db_map_base_iterator< kdt, realddt,
    ddt > &vi)
 
```

Base copy constructor.

#### Parameters

##### vi

Initialize from a base class iterator.

``` c
db_map_iterator(db_container *powner, u_int32_t b_bulk_retrieval=0,
    bool brmw=false, bool directdbget=true,
    bool b_read_only=false)
 
```

Constructor.

#### Parameters

##### b_bulk_retrieval

The bulk read buffer size. 0 means bulk read disabled.

##### brmw

Whether set DB_RMW flag in underlying cursor.

##### powner

The container which creates this iterator.

##### directdbget

Whether do direct database get rather than using key/data values cached in the iterator whenever read.

##### b_read_only

Whether open a read only cursor. Only effective when using Berkeley DB Concurrent Data Store.

``` c
db_map_iterator()
 
```

Default constructor, dose not create the cursor for now.

### Group: Constructors and destructor

Do not create iterators directly using these constructors, but call <a href="stldb_mapbegin.md" class="link" title="begin">db_map::begin</a> or db_multimap_begin to get instances of this class.

<a href="stldb_mapbegin.md" class="link" title="begin">db_map::begin()</a> <a href="stldb_mapbegin.md" class="link" title="begin">db_multimap::begin()</a>

### Class

<a href="db_map_iterator.md" class="link" title="Chapter 16.  Db_map_iterator">db_map_iterator</a>
