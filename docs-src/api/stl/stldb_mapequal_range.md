---
title: "equal_range"
api-name: "equal_range"
source: docs/api_reference/STL/stldb_mapequal_range.html
---
## equal_range

### Function Details

``` c
equal_range(const key_type &x) const
 
```

Find the range within which all keys equal to specified key x.

#### Parameters

##### x

The target key to find.

#### Return Value

The range \[first, last).

#### See Also

<a href="http://www.cplusplus.com/reference/stl/map/equal_range/" class="ulink" target="_top">http://www.cplusplus.com/reference/stl/map/equal_range/</a>

``` c
equal_range(const key_type &x,
    bool readonly=false)
 
```

Find the range within which all keys equal to specified key x.

#### Parameters

##### x

The target key to find.

##### readonly

Whether the returned iterator is readonly.

#### Return Value

The range \[first, last).

#### See Also

<a href="http://www.cplusplus.com/reference/stl/map/equal_range/" class="ulink" target="_top">http://www.cplusplus.com/reference/stl/map/equal_range/</a>

### Group: Searching Functions

The following functions are returning iterators, and they by default return read-write iterators.

If you intend to use the returned iterator only to read, you should call the const version of each function using a const reference to this container. Using const iterators can potentially promote concurrency a lot. You can also set the readonly parameter to each non-const version of the functions to true if you don't use the returned iterator to write, which also promotes concurrency and overall performance.

### Class

<a href="db_map.md" class="link" title="Chapter 5.  Db_map">db_map</a>
