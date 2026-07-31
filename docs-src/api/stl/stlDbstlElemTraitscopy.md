---
title: "copy"
api-name: "copy"
source: docs/api_reference/STL/stlDbstlElemTraitscopy.html
---
## copy

### Function Details

``` c
static T* copy(T *seq1, const T *seq2,
    size_t cnt)
 
```

Copy first cnt number of elements from seq2 to seq1.

### Group: Interface compatible with std::string's char_traits.

Following are char_traits funcitons, which make this class char_traits compatiable, so that it can be used in std::basic_string template, and be manipulated by the c++ stl algorithms.

### Class

<a href="DbstlElemTraits.md" class="link" title="Chapter 26.  DbstlElemTraits">DbstlElemTraits</a>
