---
title: "operator[]"
api-name: "operator[]"
source: docs/api_reference/STL/stldb_reverse_iteratoroperator_sqbrk.html
---
## operator\[\]

### Function Details

``` c
value_type_wrap operator[](difference_type Off) const
 
```

Return the reference of the element which can be reached by moving this reverse iterator by Off times backward.

If Off is negative, the movement will be forward.

### Class

<a href="db_reverse_iterator.md" class="link" title="Chapter 20.  Db_reverse_iterator">db_reverse_iterator</a>
