---
title: "insert"
api-name: "insert"
source: docs/api_reference/STL/stldb_vectorinsert.html
---
## insert

### Function Details

``` c
iterator insert(iterator pos,
    const T &x)
 
```

Insert x before position pos.

#### Parameters

##### x

The element to insert.

##### pos

The position before which to insert.

``` c
void insert(iterator pos, size_type n,
    const T &x)
 
```

Insert n number of elements x before position pos.

#### Parameters

##### x

The element to insert.

##### pos

The position before which to insert.

##### n

The number of elements to insert.

``` c
void insert(iterator pos, InputIterator first,
    InputIterator last)
 
```

Range insertion.

Insert elements in range \[first, last) into this vector before position pos.

#### Parameters

##### last

The open boundary of the range.

##### pos

The position before which to insert.

##### first

The closed boundary of the range.

``` c
void insert(iterator pos, const_iterator first,
    const_iterator last)
 
```

Range insertion.

Insert elements in range \[first, last) into this vector before position pos.

#### Parameters

##### last

The open boundary of the range.

##### pos

The position before which to insert.

##### first

The closed boundary of the range.

### Group: Insert functions

The iterator pos in the functions must be a read-write iterator, can't be read only.

<a href="http://www.cplusplus.com/reference/stl/vector/insert/" class="ulink" target="_top">http://www.cplusplus.com/reference/stl/vector/insert/</a>

### Class

<a href="db_vector.md" class="link" title="Chapter 4.  Db_vector">db_vector</a>
