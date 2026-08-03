---
title: "operator=="
api-name: "operator=="
source: docs/api_reference/STL/stldb_multisetoperator_eq.html
---
## operator==

### Function Details

``` c
bool operator==(const self &m2) const
 
```

Container content equality compare operator.

This function does not rely on key order. Two sets A and B are equal if and only if for each and every key K having n occurrences in A, K has n occurrences in B, and for each and every key K\` having N occurrences in B, K\` has n occurrences in A.

#### Parameters

##### m2

The container to compare against.

#### Return Value

Returns true if the two containers are equal, false otherwise.

### Class

<a href="db_multiset.md" class="link" title="Chapter 8.  Db_multiset">db_multiset</a>
