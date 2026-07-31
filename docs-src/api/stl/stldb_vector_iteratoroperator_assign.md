---
title: "operator="
api-name: "operator="
source: docs/api_reference/STL/stldb_vector_iteratoroperator_assign.html
---
## operator=

### Function Details

``` c
const self& operator=(const self &itr)
 
```

Assignment operator.

This iterator will point to the same key/data pair as itr, and have the same configurations as itr.

#### Parameters

##### itr

The right value of the assignment.

#### Return Value

This iterator's reference.

#### See Also

<a href="stldb_base_iteratoroperator_assign.md" class="link" title="operator=">db_base_iterator::operator=(const self&amp;)</a>

### Group: Iterator movement operators.

These functions have identical behaviors and semantics as those of <a href="db_vector_base_iterator.md" class="link" title="Chapter 12.  Db_vector_base_iterator">db_vector_base_iterator</a> , so please refer to equivalent in that class.

### Class

<a href="db_vector_iterator.md" class="link" title="Chapter 13.  Db_vector_iterator">db_vector_iterator</a>
