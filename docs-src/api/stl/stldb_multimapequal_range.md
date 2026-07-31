---
title: "equal_range"
api-name: "equal_range"
source: docs/api_reference/STL/stldb_multimapequal_range.html
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

<a href="http://www.cplusplus.com/reference/stl/multimap/equal_range/" class="ulink" target="_top">http://www.cplusplus.com/reference/stl/multimap/equal_range/</a>

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

<a href="http://www.cplusplus.com/reference/stl/multimap/equal_range/" class="ulink" target="_top">http://www.cplusplus.com/reference/stl/multimap/equal_range/</a>

### Group: Searching Functions

See of db_map's searching functions group for details about iterator, function version and parameters.

<a href="db_map.md" class="link" title="Chapter 5.  Db_map">db_map</a>

### Class

<a href="db_multimap.md" class="link" title="Chapter 6.  Db_multimap">db_multimap</a>
