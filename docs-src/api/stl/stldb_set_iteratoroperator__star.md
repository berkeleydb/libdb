---
title: "operator *"
api-name: "operator *"
source: docs/api_reference/STL/stldb_set_iteratoroperator__star.html
---
## operator \*

### Function Details

``` c
reference operator *()
 
```

Dereference operator.

Return the reference to the cached data element, which is an ElementRef\<T\> object if T is a class type or an ElementHolder\<T\> object if T is a C++ primitive data type.

#### Return Value

Current data element reference object, i.e. <a href="ElementHolder.md" class="link" title="Chapter 23.  ElementHolder">ElementHolder</a> or <a href="ElementRef.md" class="link" title="Chapter 24.  ElementRef">ElementRef</a> object.

### Class

<a href="db_set_iterator.md" class="link" title="Chapter 19.  Db_set_iterator">db_set_iterator</a>
