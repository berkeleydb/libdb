---
title: "operator--"
api-name: "operator--"
source: docs/api_reference/STL/stlElementHolderoperator_decr.html
---
## operator--

### Function Details

``` c
self& operator--()
 
```

``` c
self operator--(int)
 
```

### Group: Math operators.

<a href="ElementHolder.md" class="link" title="Chapter 23.  ElementHolder">ElementHolder</a> class templates also have all C/C++ self mutating operators for numeric primitive types, including: +=, -=, \*=, /=, =, \<\<=, \>\>=, &=, \|=, ^=, ++, -- These operators should not be used when ddt is a sequence pointer type like char\* or wchar_t\* or T\*, otherwise the behavior is undefined.

These methods exist only to override default bahavior to store the new updated value, otherwise, the type convert operator could have done all the job. As you know, some of them are not applicable to float or double types or <a href="ElementHolder.md" class="link" title="Chapter 23.  ElementHolder">ElementHolder</a> wrapper types for float/double types. These operators not only modifies the cached data element, but also stores new value to database if it associates a database key/data pair.

### Class

<a href="ElementHolder.md" class="link" title="Chapter 23.  ElementHolder">ElementHolder</a>
