---
title: "equal_range_N"
api-name: "equal_range_N"
source: docs/api_reference/STL/stldb_multimapequal_range_N.html
---
## equal_range_N

### Function Details

``` c
equal_range_N(const key_type &x,
    size_t &nelem) const
 
```

Find equal range and number of key/data pairs in the range.

This function also returns the number of elements within the returned range via the out parameter nelem.

#### Parameters

##### x

The target key to find.

##### nelem

The output parameter to take back the number of key/data pair in the returned range.

#### See Also

<a href="http://www.cplusplus.com/reference/stl/multimap/equal_range/" class="ulink" target="_top">http://www.cplusplus.com/reference/stl/multimap/equal_range/</a>

``` c
equal_range_N(const key_type &x, size_t &nelem,
    bool readonly=false)
 
```

Find equal range and number of key/data pairs in the range.

This function also returns the number of elements within the returned range via the out parameter nelem.

#### Parameters

##### x

The target key to find.

##### nelem

The output parameter to take back the number of key/data pair in the returned range.

##### readonly

Whether the returned iterator is readonly.

#### See Also

<a href="http://www.cplusplus.com/reference/stl/multimap/equal_range/" class="ulink" target="_top">http://www.cplusplus.com/reference/stl/multimap/equal_range/</a>

### Group: Searching Functions

See of db_map's searching functions group for details about iterator, function version and parameters.

<a href="db_map.md" class="link" title="Chapter 5.  Db_map">db_map</a>

### Class

<a href="db_multimap.md" class="link" title="Chapter 6.  Db_multimap">db_multimap</a>
