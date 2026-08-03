---
title: "move"
api-name: "move"
source: docs/api_reference/STL/stlDbstlElemTraitsmove.html
---
## move

### Function Details

``` c
static T* move(T *seq1, const T *seq2,
    size_t cnt)
 
```

Sequence movement.

Move first cnt number of elements from seq2 to seq1, seq1 and seq2 may or may not overlap.

### Group: Interface compatible with std::string's char_traits.

Following are char_traits funcitons, which make this class char_traits compatiable, so that it can be used in std::basic_string template, and be manipulated by the c++ stl algorithms.

### Class

<a href="DbstlElemTraits.md" class="link" title="Chapter 26.  DbstlElemTraits">DbstlElemTraits</a>
