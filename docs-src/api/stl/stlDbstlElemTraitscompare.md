---
title: "compare"
api-name: "compare"
source: docs/api_reference/STL/stlDbstlElemTraitscompare.html
---
## compare

### Function Details

``` c
static int compare(const T *seq1, const T *seq2,
    size_t cnt)
 
```

Sequence comparison.

Compares the first cnt number of elements in the two sequences seq1 and seq2, returns negative/0/positive if seq1 is less/equal/greater than seq2.

### Group: Interface compatible with std::string's char_traits.

Following are char_traits funcitons, which make this class char_traits compatiable, so that it can be used in std::basic_string template, and be manipulated by the c++ stl algorithms.

### Class

<a href="DbstlElemTraits.md" class="link" title="Chapter 26.  DbstlElemTraits">DbstlElemTraits</a>
