---
title: "operator=="
api-name: "operator=="
source: docs/api_reference/STL/stldb_setoperator_eq.html
---
## operator==

### Function Details

``` c
bool operator==(const db_set< kdt,
    value_type_sub > &m2) const
 
```

Set content equality comparison operator.

Return if the two containers have identical content. This function does not rely on key order. Two sets A and B are equal if and only if A contains B and B contains A.

#### Parameters

##### m2

The container to compare against.

#### Return Value

Returns true if the two containers are equal, false otherwise.

### Class

<a href="db_set.md" class="link" title="Chapter 7.  Db_set">db_set</a>
