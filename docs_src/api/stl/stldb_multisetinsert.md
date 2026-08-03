---
title: "insert"
api-name: "insert"
source: docs/api_reference/STL/stldb_multisetinsert.html
---
## insert

### Function Details

``` c
iterator insert(const value_type &x)
 
```

Insert a single key if the key is not in the container.

#### Parameters

##### x

The key to insert.

#### Return Value

An iterator positioned on the newly inserted key. If the key x already exists, an invalid iterator equal to that returned by <a href="stldb_mapend.md" class="link" title="end">end()</a> function is returned.

``` c
iterator insert(iterator position,
    const value_type &x)
 
```

Insert a single key with hint if the key is not in the container.

The hint position is ignored because Berkeley DB controls where to insert the key.

#### Parameters

##### x

The key to insert.

##### position

The hint insert position, ignored.

#### Return Value

An iterator positioned on the newly inserted key. If the key x already exists, an invalid iterator equal to that returned by <a href="stldb_mapend.md" class="link" title="end">end()</a> function is returned.

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
void insert(db_set_iterator< kdt, value_type_sub > &first,
    db_set_iterator< kdt,
    value_type_sub > &last)
 
```

Range insertion.

Insert a range \[first, last) of key/data pairs into this container.

#### Parameters

##### last

The open boundary of the range.

##### first

The closed boundary of the range.

``` c
void insert(db_set_base_iterator< kdt > &first,
    db_set_base_iterator< kdt > &last)
 
```

Range insertion.

Insert a range \[first, last) of key/data pairs into this container.

#### Parameters

##### last

The open boundary of the range.

##### first

The closed boundary of the range.

### Group: Insert Functions

<a href="http://www.cplusplus.com/reference/stl/multiset/insert/" class="ulink" target="_top">http://www.cplusplus.com/reference/stl/multiset/insert/</a>

### Class

<a href="db_multiset.md" class="link" title="Chapter 8.  Db_multiset">db_multiset</a>
