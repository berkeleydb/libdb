---
title: "max_size"
api-name: "max_size"
source: docs/api_reference/STL/stldb_vectormax_size.html
---
## max_size

### Function Details

``` c
size_type max_size() const
 
```

Get max size.

The returned size is not the actual limit of database. See the Berkeley DB limits to get real max size.

#### Return Value

A meaningless huge number.

### Group: Huge return

These two functions return 2^30, denoting a huge number that does not overflow, because dbstl does not have to manage memory space.

But the return value is not the real limit, see the Berkeley DB database limits for the limits.

### Class

<a href="db_vector.md" class="link" title="Chapter 4.  Db_vector">db_vector</a>
