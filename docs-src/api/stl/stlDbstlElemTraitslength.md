---
title: "length"
api-name: "length"
source: docs/api_reference/STL/stlDbstlElemTraitslength.html
---
## length

### Function Details

``` c
static size_t length(const T *seq)
 
```

Returns the number of elements in sequence seq1.

Note that seq1 may or may not end with a trailing '', it is completely user's responsibility for this decision, though seq\[0\], seq\[1\],... seq\[length - 1\] are all sequence seq's memory.

### Group: Interface compatible with std::string's char_traits.

Following are char_traits funcitons, which make this class char_traits compatiable, so that it can be used in std::basic_string template, and be manipulated by the c++ stl algorithms.

### Class

<a href="DbstlElemTraits.md" class="link" title="Chapter 26.  DbstlElemTraits">DbstlElemTraits</a>
