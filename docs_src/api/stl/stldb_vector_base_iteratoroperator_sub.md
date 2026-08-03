---
title: "operator-"
api-name: "operator-"
source: docs/api_reference/STL/stldb_vector_base_iteratoroperator_sub.html
---
## operator-

### Function Details

``` c
self operator-(difference_type n) const
 
```

Iterator movement operator.

Return another iterator by moving this iterator backward by n elements.

#### Parameters

##### n

The amount and direction of movement. If negative, will move backward by \|n\| element.

#### Return Value

The new iterator at new position.

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

### Group: Iterator movement operators.

When we talk about iterator movement, we think the container is a uni-directional range, represented by \[begin, end), and this is true no matter we are using iterators or reverse iterators.

When an iterator is moved closer to "begin", we say it is moved forward, otherwise we say it is moved backward.

### Class

<a href="db_vector_base_iterator.md" class="link" title="Chapter 12.  Db_vector_base_iterator">db_vector_base_iterator</a>
