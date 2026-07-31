---
title: "operator="
api-name: "operator="
source: docs/api_reference/STL/stldb_vector_base_iteratoroperator_assign.html
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

<a href="stldb_base_iteratoroperator_assign.md" class="link" title="operator=">db_base_iterator::operator=</a>

### Group: Iterator movement operators.

When we talk about iterator movement, we think the container is a uni-directional range, represented by \[begin, end), and this is true no matter we are using iterators or reverse iterators.

When an iterator is moved closer to "begin", we say it is moved forward, otherwise we say it is moved backward.

### Class

<a href="db_vector_base_iterator.md" class="link" title="Chapter 12.  Db_vector_base_iterator">db_vector_base_iterator</a>
