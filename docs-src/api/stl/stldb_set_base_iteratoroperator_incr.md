---
title: "operator++"
api-name: "operator++"
source: docs/api_reference/STL/stldb_set_base_iteratoroperator_incr.html
---
## operator++

### Function Details

``` c
self& operator++()
 
```

Post-increment.

#### Return Value

This iterator after incremented.

#### See Also

<a href="stldb_map_base_iteratoroperator_incr.md" class="link" title="operator++">db_map_base_iterator::operator++()</a>

``` c
self operator++(int)
 
```

Pre-increment.

#### Return Value

Another iterator having the old value of this iterator.

#### See Also

<a href="stldb_map_base_iteratoroperator_incr.md" class="link" title="operator++">db_map_base_iterator::operator++(int)</a>

### Group: Iterator movement operators.

These functions are identical to those of <a href="db_map_base_iterator.md" class="link" title="Chapter 15.  Db_map_base_iterator">db_map_base_iterator</a> and <a href="db_map_iterator.md" class="link" title="Chapter 16.  Db_map_iterator">db_map_iterator</a> and <a href="db_set_iterator.md" class="link" title="Chapter 19.  Db_set_iterator">db_set_iterator</a> .

Actually the iterator movement functions in the four classes are the same.

### Class

<a href="db_set_base_iterator.md" class="link" title="Chapter 18.  Db_set_base_iterator">db_set_base_iterator</a>
