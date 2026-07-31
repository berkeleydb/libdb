---
title: "operator-="
api-name: "operator-="
source: docs/api_reference/STL/stldb_vector_iteratoroperator_sa.html
---
## operator-=

### Function Details

``` c
const self& operator-=(difference_type n)
 
```

Move this iterator forward by n elements.

#### Parameters

##### n

The amount and direction of movement. If negative, will move backward by \|n\| element.

#### Return Value

Reference to this iterator at new position.

#### See Also

<a href="stldb_vector_base_iteratoroperator_sa.md" class="link" title="operator-=">db_vector_base_iterator::operator-=(difference_type n)</a>

### Group: Iterator movement operators.

These functions have identical behaviors and semantics as those of <a href="db_vector_base_iterator.md" class="link" title="Chapter 12.  Db_vector_base_iterator">db_vector_base_iterator</a> , so please refer to equivalent in that class.

### Class

<a href="db_vector_iterator.md" class="link" title="Chapter 13.  Db_vector_iterator">db_vector_iterator</a>
