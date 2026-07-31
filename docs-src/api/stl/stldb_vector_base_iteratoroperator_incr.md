---
title: "operator++"
api-name: "operator++"
source: docs/api_reference/STL/stldb_vector_base_iteratoroperator_incr.html
---
## operator++

### Function Details

``` c
self& operator++()
 
```

Pre-increment.

Move the iterator one element backward, so that the element it sits on has a bigger index. Use ++iter rather than iter++ where possible to avoid two useless iterator copy constructions.

#### Return Value

This iterator after incremented.

``` c
self operator++(int)
 
```

Post-increment.

Move the iterator one element backward, so that the element it sits on has a bigger index. Use ++iter rather than iter++ where possible to avoid two useless iterator copy constructions.

#### Return Value

A new iterator not incremented.

### Group: Iterator movement operators.

When we talk about iterator movement, we think the container is a uni-directional range, represented by \[begin, end), and this is true no matter we are using iterators or reverse iterators.

When an iterator is moved closer to "begin", we say it is moved forward, otherwise we say it is moved backward.

### Class

<a href="db_vector_base_iterator.md" class="link" title="Chapter 12.  Db_vector_base_iterator">db_vector_base_iterator</a>
