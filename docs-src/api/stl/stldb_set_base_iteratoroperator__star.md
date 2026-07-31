---
title: "operator *"
api-name: "operator *"
source: docs/api_reference/STL/stldb_set_base_iteratoroperator__star.html
---
## operator \*

### Function Details

``` c
reference operator *()
 
```

Dereference operator.

Return the reference to the cached data element, which is an object of type T. You can only use the return value to read its referenced data element, can not update it.

#### Return Value

Current data element reference object, i.e. <a href="ElementHolder.md" class="link" title="Chapter 23.  ElementHolder">ElementHolder</a> or <a href="ElementRef.md" class="link" title="Chapter 24.  ElementRef">ElementRef</a> object.

### Class

<a href="db_set_base_iterator.md" class="link" title="Chapter 18.  Db_set_base_iterator">db_set_base_iterator</a>
