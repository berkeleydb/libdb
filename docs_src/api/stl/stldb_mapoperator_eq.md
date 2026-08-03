---
title: "operator=="
api-name: "operator=="
source: docs/api_reference/STL/stldb_mapoperator_eq.html
---
## operator==

### Function Details

``` c
bool operator==(const db_map< kdt, ddt,
    value_type_sub > &m2) const
 
```

Map content equality comparison operator.

This function does not rely on key order. For a set of keys S1 in this container and another set of keys S2 of container m2, if set S1 contains S2 and S2 contains S1 (S1 equals to S2) and each data element of a key K in S1 from this container equals the data element of K in m2, the two db_map\<\> containers equal. Otherwise they are not equal.

#### Parameters

##### m2

The other container to compare against.

#### Return Value

Returns true if they have equal content, false otherwise.

### Class

<a href="db_map.md" class="link" title="Chapter 5.  Db_map">db_map</a>
