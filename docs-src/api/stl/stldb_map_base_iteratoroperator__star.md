---
title: "operator *"
api-name: "operator *"
source: docs/api_reference/STL/stldb_map_base_iteratoroperator__star.html
---
## operator \*

### Function Details

``` c
reference operator *() const
 
```

Dereference operator.

Return the reference to the cached data element, which is an pair\<Key_type, T\>. You can only read its referenced data via this iterator but can not update it.

#### Return Value

Current data element reference object, i.e. <a href="ElementHolder.md" class="link" title="Chapter 23.  ElementHolder">ElementHolder</a> or <a href="ElementRef.md" class="link" title="Chapter 24.  ElementRef">ElementRef</a> object.

### Class

<a href="db_map_base_iterator.md" class="link" title="Chapter 15.  Db_map_base_iterator">db_map_base_iterator</a>
