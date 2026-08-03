---
title: "operator-"
api-name: "operator-"
source: docs/api_reference/STL/stldb_vector_iteratoroperator_sub.html
---
## operator-

### Function Details

``` c
self operator-(difference_type n) const
 
```

Iterator movement operator.

Return another iterator by moving this iterator forward by n elements.

#### Parameters

##### n

The amount and direction of movement. If negative, will move backward by \|n\| element.

#### Return Value

The new iterator at new position.

#### See Also

<a href="stldb_vector_base_iteratoroperator_sub.md" class="link" title="operator-">db_vector_base_iterator::operator-(difference_type n) const</a>

``` c
difference_type operator-(const self &itr) const
 
```

Iterator distance operator.

Return the index difference of this iterator and itr, so if this iterator sits on an element with a smaller index, this call will return a negative number.

#### Parameters

##### itr

The other iterator to substract. itr can be the invalid iterator after last element or before first element, their index will be regarded as last element's index + 1 and -1 respectively.

#### Return Value

The index difference.

#### See Also

<a href="stldb_vector_base_iteratoroperator_sub.md" class="link" title="operator-">db_vector_base_iterator::operator-(const self&amp; itr) const</a>

### Group: Iterator movement operators.

These functions have identical behaviors and semantics as those of <a href="db_vector_base_iterator.md" class="link" title="Chapter 12.  Db_vector_base_iterator">db_vector_base_iterator</a> , so please refer to equivalent in that class.

### Class

<a href="db_vector_iterator.md" class="link" title="Chapter 13.  Db_vector_iterator">db_vector_iterator</a>
