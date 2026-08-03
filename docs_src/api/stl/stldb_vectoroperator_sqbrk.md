---
title: "operator[]"
api-name: "operator[]"
source: docs/api_reference/STL/stldb_vectoroperator_sqbrk.html
---
## operator\[\]

### Function Details

``` c
reference operator[](index_type n)
 
```

Index operator, can act as both a left value and a right value.

#### Parameters

##### n

The valid index of the vector.

#### Return Value

The reference to the element at specified position.

``` c
const_reference operator[](index_type n) const
 
```

Read only index operator.

Only used as a right value, no need for assignment capability. The return value can't be used to update the element.

#### Parameters

##### n

The valid index of the vector.

#### Return Value

The const reference to the element at specified position.

### Group: Element access functions.

The operator\[\] and <a href="stldb_vectorat.md" class="link" title="at">at()</a> only come from std::vector and std::deque, If you are using <a href="db_vector.md" class="link" title="Chapter 4.  Db_vector">db_vector</a> as std::list, you don't have to set DB_RENUMBER flag to the backing database handle, and you get better performance, but at the same time you can't use these functions.

Otherwise if you have set the DB_RENUMBER flag to the backing database handle, you can use this function though it is an std::list equivalent.

### Class

<a href="db_vector.md" class="link" title="Chapter 4.  Db_vector">db_vector</a>
