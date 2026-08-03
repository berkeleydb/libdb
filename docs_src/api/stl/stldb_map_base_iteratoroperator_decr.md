---
title: "operator--"
api-name: "operator--"
source: docs/api_reference/STL/stldb_map_base_iteratoroperator_decr.html
---
## operator--

### Function Details

``` c
self& operator--()
 
```

Pre-decrement.

#### Return Value

This iterator after decremented.

``` c
self operator--(int)
 
```

Post-decrement.

#### Return Value

Another iterator having the old value of this iterator.

### Group: Iterator decrement movement functions.

The two functions moves the iterator one element forward, so that the element it sits on has a smaller key.

The btree/hash key comparison routine determines which key is greater. Use --iter rather than iter-- where possible to avoid two useless iterator copy constructions.

### Class

<a href="db_map_base_iterator.md" class="link" title="Chapter 15.  Db_map_base_iterator">db_map_base_iterator</a>
