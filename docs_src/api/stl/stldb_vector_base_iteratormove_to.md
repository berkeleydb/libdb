---
title: "move_to"
api-name: "move_to"
source: docs/api_reference/STL/stldb_vector_base_iteratormove_to.html
---
## move_to

### Function Details

``` c
void move_to(index_type n) const
 
```

Iterator movement function.

Move this iterator to the index "n". If n is not in the valid range, this iterator will be an invalid iterator equal to end() iterator.

#### Parameters

##### n

target element's index.

#### See Also

<a href="stldb_vectorend.md" class="link" title="end">db_vector::end()</a> ;

### Class

<a href="db_vector_base_iterator.md" class="link" title="Chapter 12.  Db_vector_base_iterator">db_vector_base_iterator</a>
