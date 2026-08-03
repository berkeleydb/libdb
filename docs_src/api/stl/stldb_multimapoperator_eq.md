---
title: "operator=="
api-name: "operator=="
source: docs/api_reference/STL/stldb_multimapoperator_eq.html
---
## operator==

### Function Details

``` c
bool operator==(const db_multimap< kdt, ddt,
    value_type_sub > &m2) const
 
```

Returns whether the two containers have identical content.

This function does not rely on key order. For a set of keys S1 in this container and another set of keys S2 of container m2, if set S1 contains S2 and S2 contains S1 (S1 equals to S2) and each set of data elements of any key K in S1 from this container equals the set of data elements of K in m2, the two db_multimap\<\> containers equal. Otherwise they are not equal. Data element set comparison does not rely on order either.

#### Parameters

##### m2

The other container to compare against.

#### Return Value

Returns true if they are equal, false otherwise.

### Class

<a href="db_multimap.md" class="link" title="Chapter 6.  Db_multimap">db_multimap</a>
