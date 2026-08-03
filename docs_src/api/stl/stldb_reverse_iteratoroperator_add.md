---
title: "operator+"
api-name: "operator+"
source: docs/api_reference/STL/stldb_reverse_iteratoroperator_add.html
---
## operator+

### Function Details

``` c
self operator+(difference_type n) const
 
```

Iterator shuffle operator.

Return a new iterator by moving this iterator forward by n elements.

#### Parameters

##### n

The amount and direction of movement. If negative, will move towards reverse direction.

#### Return Value

A new iterator at new position.

### Group: Operators for random reverse iterators

Methods below only applies to random iterators.

/////

Return a new iterator by moving this iterator backward or forward by n elements.

### Class

<a href="db_reverse_iterator.md" class="link" title="Chapter 20.  Db_reverse_iterator">db_reverse_iterator</a>
