---
title: "operator *"
api-name: "operator *"
source: docs/api_reference/STL/stldb_vector_base_iteratoroperator__star.html
---
## operator \*

### Function Details

``` c
reference operator *() const
 
```

Dereference operator.

Return the reference to the cached data element, which is an ElementRef\<T\> object if T is a class type or an ElementHolder\<T\> object if T is a C++ primitive data type. The returned value can only be used to read its referenced element.

#### Return Value

The reference to the element this iterator points to.

### Class

<a href="db_vector_base_iterator.md" class="link" title="Chapter 12.  Db_vector_base_iterator">db_vector_base_iterator</a>
