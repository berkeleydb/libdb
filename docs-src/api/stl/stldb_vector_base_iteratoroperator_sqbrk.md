---
title: "operator[]"
api-name: "operator[]"
source: docs/api_reference/STL/stldb_vector_base_iteratoroperator_sqbrk.html
---
## operator\[\]

### Function Details

``` c
value_type_wrap operator[](difference_type _Off) const
 
```

Iterator index operator.

If \_Off not in a valid range, the returned value will be invalid. Note that you should use a value_type_wrap type to hold the returned value.

#### Parameters

##### \_Off

The valid index relative to this iterator.

#### Return Value

Return the element which is at position \*this + \_Off. The returned value can only be used to read its referenced element.

### Class

<a href="db_vector_base_iterator.md" class="link" title="Chapter 12.  Db_vector_base_iterator">db_vector_base_iterator</a>
