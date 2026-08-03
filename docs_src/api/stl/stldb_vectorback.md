---
title: "back"
api-name: "back"
source: docs/api_reference/STL/stldb_vectorback.html
---
## back

### Function Details

``` c
reference back()
 
```

Return a reference to the last element.

#### Return Value

Return a reference to the last element.

#### See Also

<a href="http://www.cplusplus.com/reference/stl/vector/back/" class="ulink" target="_top">http://www.cplusplus.com/reference/stl/vector/back/</a>

``` c
const_reference back() const
 
```

Return a reference to the last element.

The return value can't be used to update the element.

#### Return Value

Return a reference to the last element.

#### See Also

<a href="http://www.cplusplus.com/reference/stl/vector/back/" class="ulink" target="_top">http://www.cplusplus.com/reference/stl/vector/back/</a>

### Group: Element access functions.

The operator\[\] and <a href="stldb_vectorat.md" class="link" title="at">at()</a> only come from std::vector and std::deque, If you are using <a href="db_vector.md" class="link" title="Chapter 4.  Db_vector">db_vector</a> as std::list, you don't have to set DB_RENUMBER flag to the backing database handle, and you get better performance, but at the same time you can't use these functions.

Otherwise if you have set the DB_RENUMBER flag to the backing database handle, you can use this function though it is an std::list equivalent.

### Class

<a href="db_vector.md" class="link" title="Chapter 4.  Db_vector">db_vector</a>
