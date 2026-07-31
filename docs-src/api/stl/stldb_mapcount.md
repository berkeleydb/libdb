---
title: "count"
api-name: "count"
source: docs/api_reference/STL/stldb_mapcount.html
---
## count

### Function Details

``` c
size_type count(const key_type &x) const
 
```

Count the number of key/data pairs having specified key x.

#### Parameters

##### x

The key to count.

#### Return Value

The number of key/data pairs having x as key within the container.

#### See Also

<a href="http://www.cplusplus.com/reference/stl/map/count/" class="ulink" target="_top">http://www.cplusplus.com/reference/stl/map/count/</a>

### Group: Searching Functions

The following functions are returning iterators, and they by default return read-write iterators.

If you intend to use the returned iterator only to read, you should call the const version of each function using a const reference to this container. Using const iterators can potentially promote concurrency a lot. You can also set the readonly parameter to each non-const version of the functions to true if you don't use the returned iterator to write, which also promotes concurrency and overall performance.

### Class

<a href="db_map.md" class="link" title="Chapter 5.  Db_map">db_map</a>
