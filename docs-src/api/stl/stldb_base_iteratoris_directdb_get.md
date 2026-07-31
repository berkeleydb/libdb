---
title: "is_directdb_get"
api-name: "is_directdb_get"
source: docs/api_reference/STL/stldb_base_iteratoris_directdb_get.html
---
## is_directdb_get

### Function Details

``` c
bool is_directdb_get() const
 
```

Get direct database get setting.

Return true if every operation to retrieve the key/data pair the iterator points to will read from database rather than using the cached value, false otherwise.

### Class

<a href="db_base_iterator.md" class="link" title="Chapter 10.  Db_base_iterator">db_base_iterator</a>
