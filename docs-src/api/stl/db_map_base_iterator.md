---
title: "Chapter 15.  Db_map_base_iterator"
api-name: "Chapter 15.  Db_map_base_iterator"
source: docs/api_reference/STL/db_map_base_iterator.html
---
## Chapter 15.  Db_map_base_iterator

#### Public Members

| Member | Description |
|----|----|
| <a href="db_map_base_iterator.md#stldb_map_base_iteratordb_map_base_iterator" class="xref" title="db_map_base_iterator">db_map_base_iterator</a> | Copy constructor. |
| <a href="stldb_map_base_iteratordstr_db_map_base_iterator.md" class="xref" title="~db_map_base_iterator">~db_map_base_iterator</a> | Destructor. |
| <a href="stldb_map_base_iteratoroperator_incr.md" class="xref" title="operator++">operator++</a> | Pre-increment. |
| <a href="stldb_map_base_iteratoroperator_decr.md" class="xref" title="operator--">operator--</a> | Pre-decrement. |
| <a href="stldb_map_base_iteratoroperator_eq.md" class="xref" title="operator==">operator==</a> | Equal comparison operator. |
| <a href="stldb_map_base_iteratoroperator_ueq.md" class="xref" title="operator!=">operator!=</a> | Unequal comparison operator. |
| <a href="stldb_map_base_iteratoroperator__star.md" class="xref" title="operator *">operator *</a> | Dereference operator. |
| <a href="stldb_map_base_iteratoroperator_arrow.md" class="xref" title="operator-&gt;">operator-&gt;</a> | Arrow operator. |
| <a href="stldb_map_base_iteratorrefresh.md" class="xref" title="refresh">refresh</a> | Refresh iterator cached value. |
| <a href="stldb_map_base_iteratorclose_cursor.md" class="xref" title="close_cursor">close_cursor</a> | Close underlying Berkeley DB cursor of this iterator. |
| <a href="stldb_map_base_iteratormove_to.md" class="xref" title="move_to">move_to</a> | Iterator movement function. |
| <a href="stldb_map_base_iteratorset_bulk_buffer.md" class="xref" title="set_bulk_buffer">set_bulk_buffer</a> | Modify bulk buffer size. |
| <a href="stldb_map_base_iteratorget_bulk_bufsize.md" class="xref" title="get_bulk_bufsize">get_bulk_bufsize</a> | Get bulk retrieval buffer size in bytes. |
| <a href="stldb_map_base_iteratoroperator_assign.md" class="xref" title="operator=">operator=</a> | Assignment operator. |

#### Group

<a href="db_map_iterators.md" class="xref" title="Chapter 14.  Iterator Classes for db_map and db_multimap">Iterator Classes for db_map and db_multimap</a>

## db_map_base_iterator

### Function Details

``` c
db_map_base_iterator(const self &vi)
 
```

Copy constructor.

#### Parameters

##### vi

The other iterator of the same type to initialize this.

``` c
db_map_base_iterator(const base &vi)
 
```

Base copy constructor.

#### Parameters

##### vi

Initialize from a base class iterator.

``` c
db_map_base_iterator(db_container *powner, u_int32_t b_bulk_retrieval=0,
    bool rmw=false, bool directdbget=true,
    bool readonly=false)
 
```

Constructor.

#### Parameters

##### b_bulk_retrieval

The bulk read buffer size. 0 means bulk read disabled.

##### directdbget

Whether do direct database get rather than using key/data values cached in the iterator whenever read.

##### readonly

Whether open a read only cursor. Only effective when using Berkeley DB Concurrent Data Store.

##### powner

The container which creates this iterator.

##### rmw

Whether set DB_RMW flag in underlying cursor.

``` c
db_map_base_iterator()
 
```

Default constructor, dose not create the cursor for now.

### Group: Constructors and destructor

Do not create iterators directly using these constructors, but call <a href="stldb_mapbegin.md" class="link" title="begin">db_map::begin</a> or db_multimap_begin to get instances of this class.

<a href="stldb_mapbegin.md" class="link" title="begin">db_map::begin()</a> <a href="stldb_mapbegin.md" class="link" title="begin">db_multimap::begin()</a>

### Class

<a href="db_map_base_iterator.md" class="link" title="Chapter 15.  Db_map_base_iterator">db_map_base_iterator</a>
