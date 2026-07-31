---
title: "find"
api-name: "find"
source: docs/api_reference/STL/stldb_mapfind.html
---
## find

### Function Details

``` c
const_iterator find(const key_type &x) const
 
```

Find the key/data pair with specified key x.

#### Parameters

##### x

The target key to find.

#### Return Value

The valid const iterator sitting on the key x, or an invalid one.

#### See Also

<a href="http://www.cplusplus.com/reference/stl/map/find/" class="ulink" target="_top">http://www.cplusplus.com/reference/stl/map/find/</a>

``` c
iterator find(const key_type &x,
    bool readonly=false)
 
```

Find the key/data pair with specified key x.

#### Parameters

##### x

The target key to find.

##### readonly

Whether the returned iterator is readonly.

#### Return Value

The valid iterator sitting on the key x, or an invalid one.

#### See Also

<a href="http://www.cplusplus.com/reference/stl/map/find/" class="ulink" target="_top">http://www.cplusplus.com/reference/stl/map/find/</a>

### Group: Searching Functions

The following functions are returning iterators, and they by default return read-write iterators.

If you intend to use the returned iterator only to read, you should call the const version of each function using a const reference to this container. Using const iterators can potentially promote concurrency a lot. You can also set the readonly parameter to each non-const version of the functions to true if you don't use the returned iterator to write, which also promotes concurrency and overall performance.

### Class

<a href="db_map.md" class="link" title="Chapter 5.  Db_map">db_map</a>
