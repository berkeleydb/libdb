---
title: "insert"
api-name: "insert"
source: docs/api_reference/STL/stldb_setinsert.html
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

A pair P, if insert OK, i.e. the inserted key wasn't in the container, P.first will be the iterator positioned on the inserted key/data pair, and P.second is true; otherwise P.first is an invalid iterator equal to that returned by <a href="stldb_mapend.md" class="link" title="end">end()</a> and P.second is false.

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
void insert(iterator &first,
    iterator &last)
 
```

Range insertion.

Insert a range \[first, last) of key/data pairs into this container.

#### Parameters

##### last

The open boundary of the range.

##### first

The closed boundary of the range.

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

The iterator positioned on the inserted key/data pair, or an invalid iterator if the key was already in the container.

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

<a href="http://www.cplusplus.com/reference/stl/set/insert/" class="ulink" target="_top">http://www.cplusplus.com/reference/stl/set/insert/</a>

### Class

<a href="db_set.md" class="link" title="Chapter 7.  Db_set">db_set</a>
