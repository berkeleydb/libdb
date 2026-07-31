---
title: "capacity"
api-name: "capacity"
source: docs/api_reference/STL/stldb_vectorcapacity.html
---
## capacity

### Function Details

``` c
size_type capacity() const
 
```

Get capacity.

### Group: Huge return

These two functions return 2^30, denoting a huge number that does not overflow, because dbstl does not have to manage memory space.

But the return value is not the real limit, see the Berkeley DB database limits for the limits.

### Class

<a href="db_vector.md" class="link" title="Chapter 4.  Db_vector">db_vector</a>
