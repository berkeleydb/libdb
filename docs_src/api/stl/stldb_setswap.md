---
title: "swap"
api-name: "swap"
source: docs/api_reference/STL/stldb_setswap.html
---
## swap

### Function Details

``` c
void swap(db_set< kdt, value_type_sub > &mp,
    bool b_truncate=true)
 
```

Swap content with another container.

This function supports auto-commit.

#### Parameters

##### b_truncate

See <a href="stldb_vectorswap.md" class="link" title="swap">db_vector::swap</a> 's b_truncate parameter for details.

##### mp

The container to swap content with.

#### See Also

<a href="stldb_mapswap.md" class="link" title="swap">db_map::swap()</a> <a href="stldb_vectorclear.md" class="link" title="clear">db_vector::clear()</a>

### Class

<a href="db_set.md" class="link" title="Chapter 7.  Db_set">db_set</a>
