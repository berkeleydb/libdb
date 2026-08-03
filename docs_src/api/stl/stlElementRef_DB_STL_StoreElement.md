---
title: "_DB_STL_StoreElement"
api-name: "_DB_STL_StoreElement"
source: docs/api_reference/STL/stlElementRef_DB_STL_StoreElement.html
---
## \_DB_STL_StoreElement

### Function Details

``` c
void _DB_STL_StoreElement()
 
```

Function to store the data element.

The user needs to call this method after modifying the underlying object, so that the version stored in the container can be updated.

When db_base_iterator's directdb_get\_ member is true, this function must be called after modifying the data member and before any subsequent container iterator dereference operations. If this step is not carried out any changes will be lost.

If the data element is changed via ElementHolder\<\>::operator=(), you don't need to call this function.

### Class

<a href="ElementRef.md" class="link" title="Chapter 24.  ElementRef">ElementRef</a>
