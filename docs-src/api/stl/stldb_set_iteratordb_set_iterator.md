---
title: "db_set_iterator"
api-name: "db_set_iterator"
source: docs/api_reference/STL/stldb_set_iteratordb_set_iterator.html
---
## db_set_iterator

### Function Details

``` c
db_set_iterator(db_container *powner, u_int32_t b_bulk_retrieval=0,
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
db_set_iterator()
 
```

Default constructor, dose not create the cursor for now.

``` c
db_set_iterator(const db_set_iterator &s)
 
```

Copy constructor.

#### Parameters

##### s

The other iterator of the same type to initialize this.

``` c
db_set_iterator(const base &bo)
 
```

Base copy constructor.

#### Parameters

##### bo

Initialize from a base class iterator.

``` c
db_set_iterator(const db_set_base_iterator< kdt > &bs)
 
```

Sibling copy constructor.

Note that this class does not derive from <a href="db_set_base_iterator.md" class="link" title="Chapter 18.  Db_set_base_iterator">db_set_base_iterator</a> but from <a href="db_map_iterator.md" class="link" title="Chapter 16.  Db_map_iterator">db_map_iterator</a> .

#### Parameters

##### bs

Initialize from a base class iterator.

### Group: Constructors and destructor

Do not use these constructors to create iterators, but call <a href="stldb_mapbegin.md" class="link" title="begin">db_set::begin()</a> or <a href="stldb_mapbegin.md" class="link" title="begin">db_multiset::begin()</a> to create valid ones.

### Class

<a href="db_set_iterator.md" class="link" title="Chapter 19.  Db_set_iterator">db_set_iterator</a>
