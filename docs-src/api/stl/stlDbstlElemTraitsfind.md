---
title: "find"
api-name: "find"
source: docs/api_reference/STL/stlDbstlElemTraitsfind.html
---
## find

### Function Details

``` c
static const T* find(const T *seq, size_t cnt,
    const T &elem)
 
```

Find within the first cnt elements of sequence seq the position of element equal to elem.

### Group: Interface compatible with std::string's char_traits.

Following are char_traits funcitons, which make this class char_traits compatiable, so that it can be used in std::basic_string template, and be manipulated by the c++ stl algorithms.

### Class

<a href="DbstlElemTraits.md" class="link" title="Chapter 26.  DbstlElemTraits">DbstlElemTraits</a>
