---
title: "insert"
api-name: "insert"
source: docs/api_reference/STL/stldb_mapinsert.html
---
## insert

### Function Details

``` c
insert(const value_type &x)
 
```

Insert a single key/data pair if the key is not in the container.

#### Parameters

##### x

The key/data pair to insert.

#### Return Value

A pair P, if insert OK, i.e. the inserted key wasn't in the container, P.first will be the iterator sitting on the inserted key/data pair, and P.second is true; otherwise P.first is an invalid iterator and P.second is false.

``` c
iterator insert(iterator position,
    const value_type &x)
 
```

Insert with hint position.

We ignore the hint position because Berkeley DB knows better where to insert.

#### Parameters

##### position

The hint position.

##### x

The key/data pair to insert.

#### Return Value

The iterator sitting on the inserted key/data pair, or an invalid iterator if the key was already in the container.

``` c
void insert(const db_map_base_iterator< kdt, realddt, ddt > &first,
    const db_map_base_iterator< kdt, realddt,
    ddt > &last)
 
```

Range insertion.

Insert a range \[first, last) of key/data pairs into this container.

#### Parameters

##### last

The open boundary of the range.

##### first

The closed boundary of the range.

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

### Group: Insert Functions

They have similiar usage as their C++ STL equivalents.

Note that when secondary index is enabled, each <a href="db_container.md" class="link" title="Chapter 3.  Db_container">db_container</a> can create a <a href="db_multimap.md" class="link" title="Chapter 6.  Db_multimap">db_multimap</a> secondary container, but the insert function is not functional for secondary containers.

<a href="http://www.cplusplus.com/reference/stl/map/insert/" class="ulink" target="_top">http://www.cplusplus.com/reference/stl/map/insert/</a>

### Class

<a href="db_map.md" class="link" title="Chapter 5.  Db_map">db_map</a>
