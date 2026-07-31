---
title: "upper_bound"
api-name: "upper_bound"
source: docs/api_reference/STL/stldb_multimapupper_bound.html
---
## upper_bound

### Function Details

``` c
const_iterator upper_bound(const key_type &x) const
 
```

Find the least key greater than x.

#### Parameters

##### x

The target key to find.

#### Return Value

The valid iterator sitting on the key, or an invalid one.

#### See Also

<a href="http://www.cplusplus.com/reference/stl/multimap/upper_bound/" class="ulink" target="_top">http://www.cplusplus.com/reference/stl/multimap/upper_bound/</a>

``` c
iterator upper_bound(const key_type &x,
    bool readonly=false)
 
```

Find the least key greater than x.

#### Parameters

##### x

The target key to find.

##### readonly

Whether the returned iterator is readonly.

#### Return Value

The valid iterator sitting on the key, or an invalid one.

#### See Also

<a href="http://www.cplusplus.com/reference/stl/multimap/upper_bound/" class="ulink" target="_top">http://www.cplusplus.com/reference/stl/multimap/upper_bound/</a>

### Group: Searching Functions

See of db_map's searching functions group for details about iterator, function version and parameters.

<a href="db_map.md" class="link" title="Chapter 5.  Db_map">db_map</a>

### Class

<a href="db_multimap.md" class="link" title="Chapter 6.  Db_multimap">db_multimap</a>
