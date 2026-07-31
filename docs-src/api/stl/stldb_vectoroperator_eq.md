---
title: "operator=="
api-name: "operator=="
source: docs/api_reference/STL/stldb_vectoroperator_eq.html
---
## operator==

### Function Details

``` c
bool operator==(const db_vector< T2,
    T3 > &v2) const
 
```

Container equality comparison operator.

This function supports auto-commit.

#### Parameters

##### v2

The vector to compare against.

#### Return Value

Compare two vectors, return true if they have identical sequences of elements, otherwise return false.

``` c
bool operator==(const self &v2) const
 
```

Container equality comparison operator.

This function supports auto-commit.

#### Return Value

Compare two vectors, return true if they have identical elements, otherwise return false.

### Group: Compare functions.

<a href="http://www.sgi.com/tech/stl/Vector.html" class="ulink" target="_top">http://www.sgi.com/tech/stl/Vector.html</a>

### Class

<a href="db_vector.md" class="link" title="Chapter 4.  Db_vector">db_vector</a>
