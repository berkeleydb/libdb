---
title: "operator+"
api-name: "operator+"
source: docs/api_reference/STL/stldb_vector_base_iteratoroperator_add.html
---
## operator+

### Function Details

``` c
self operator+(difference_type n) const
 
```

Iterator movement operator.

Return another iterator by moving this iterator forward by n elements.

#### Parameters

##### n

The amount and direction of movement. If negative, will move forward by \|n\| element.

#### Return Value

The new iterator at new position.

### Group: Iterator movement operators.

When we talk about iterator movement, we think the container is a uni-directional range, represented by \[begin, end), and this is true no matter we are using iterators or reverse iterators.

When an iterator is moved closer to "begin", we say it is moved forward, otherwise we say it is moved backward.

### Class

<a href="db_vector_base_iterator.md" class="link" title="Chapter 12.  Db_vector_base_iterator">db_vector_base_iterator</a>
