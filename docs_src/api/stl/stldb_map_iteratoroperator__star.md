---
title: "operator *"
api-name: "operator *"
source: docs/api_reference/STL/stldb_map_iteratoroperator__star.html
---
## operator \*

### Function Details

``` c
reference operator *() const
 
```

Dereference operator.

Return the reference to the cached data element, which is an pair\<Key_type, ElementRef\<T\> \> object if T is a class type or an pair\<Key_type, ElementHolder\<T\> \> object if T is a C++ primitive data type.

#### Return Value

Current data element reference object, i.e. <a href="ElementHolder.md" class="link" title="Chapter 23.  ElementHolder">ElementHolder</a> or <a href="ElementRef.md" class="link" title="Chapter 24.  ElementRef">ElementRef</a> object.

### Class

<a href="db_map_iterator.md" class="link" title="Chapter 16.  Db_map_iterator">db_map_iterator</a>
