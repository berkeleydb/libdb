---
title: "operator-="
api-name: "operator-="
source: docs/api_reference/STL/stldb_reverse_iteratoroperator_sa.html
---
## operator-=

### Function Details

``` c
const self& operator-=(difference_type n)
 
```

Iterator shuffle operator.

Move this iterator backward by n elements and then return it.

#### Parameters

##### n

The amount and direction of movement. If negative, will move towards reverse direction.

#### Return Value

This iterator at new position.

### Group: Operators for random reverse iterators

Move this iterator backward or forward by n elements and then return it.

### Class

<a href="db_reverse_iterator.md" class="link" title="Chapter 20.  Db_reverse_iterator">db_reverse_iterator</a>
