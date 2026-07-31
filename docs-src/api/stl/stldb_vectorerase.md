---
title: "erase"
api-name: "erase"
source: docs/api_reference/STL/stldb_vectorerase.html
---
## erase

### Function Details

``` c
iterator erase(iterator pos)
 
```

Erase element at position pos.

#### Parameters

##### pos

The valid position in the container's range to erase.

#### Return Value

The next position after the erased element.

``` c
iterator erase(iterator first,
    iterator last)
 
```

Erase elements in range \[first, last).

#### Parameters

##### last

The open boundary of the range.

##### first

The closed boundary of the range.

#### Return Value

The next position after the erased elements.

### Group: Erase functions

The iterator pos in the functions must be a read-write iterator, can't be read only.

<a href="http://www.cplusplus.com/reference/stl/vector/erase/" class="ulink" target="_top">http://www.cplusplus.com/reference/stl/vector/erase/</a>

### Class

<a href="db_vector.md" class="link" title="Chapter 4.  Db_vector">db_vector</a>
