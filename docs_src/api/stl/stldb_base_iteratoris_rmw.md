---
title: "is_rmw"
api-name: "is_rmw"
source: docs/api_reference/STL/stldb_base_iteratoris_rmw.html
---
## is_rmw

### Function Details

``` c
bool is_rmw() const
 
```

Get DB_RMW setting.

Return true if the iterator's cursor has DB_RMW flag set, false otherwise. DB_RMW flag causes a write lock to be acquired when reading a key/data pair, so that the transaction won't block later when writing back the updated value in a read-modify-write operation cycle.

### Class

<a href="db_base_iterator.md" class="link" title="Chapter 10.  Db_base_iterator">db_base_iterator</a>
