---
title: "operator+"
api-name: "operator+"
source: docs/api_reference/STL/stldb_vector_iteratoroperator_add.html
---
## operator+

### Function Details

``` c
self operator+(difference_type n) const
 
```

Iterator movement operator.

Return another iterator by moving this iterator backward by n elements.

#### Parameters

##### n

The amount and direction of movement. If negative, will move forward by \|n\| element.

#### Return Value

The new iterator at new position.

#### See Also

<a href="stldb_vector_base_iteratoroperator_add.md" class="link" title="operator+">db_vector_base_iterator::operator+(difference_type n) const</a>

### Group: Iterator movement operators.

These functions have identical behaviors and semantics as those of <a href="db_vector_base_iterator.md" class="link" title="Chapter 12.  Db_vector_base_iterator">db_vector_base_iterator</a> , so please refer to equivalent in that class.

### Class

<a href="db_vector_iterator.md" class="link" title="Chapter 13.  Db_vector_iterator">db_vector_iterator</a>
