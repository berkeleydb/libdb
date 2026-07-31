---
title: "operator++"
api-name: "operator++"
source: docs/api_reference/STL/stldb_map_base_iteratoroperator_incr.html
---
## operator++

### Function Details

``` c
self& operator++()
 
```

Pre-increment.

#### Return Value

This iterator after incremented.

``` c
self operator++(int)
 
```

Post-increment.

#### Return Value

Another iterator having the old value of this iterator.

### Group: Iterator increment movement functions.

The two functions moves the iterator one element backward, so that the element it sits on has a bigger key.

The btree/hash key comparison routine determines which key is greater. Use ++iter rather than iter++ where possible to avoid two useless iterator copy constructions.

### Class

<a href="db_map_base_iterator.md" class="link" title="Chapter 15.  Db_map_base_iterator">db_map_base_iterator</a>
