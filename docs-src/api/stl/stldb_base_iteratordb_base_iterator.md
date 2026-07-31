---
title: "db_base_iterator"
api-name: "db_base_iterator"
source: docs/api_reference/STL/stldb_base_iteratordb_base_iterator.html
---
## db_base_iterator

### Function Details

``` c
db_base_iterator()
 
```

Default constructor.

``` c
db_base_iterator(db_container *powner, bool directdbget, bool b_read_only,
    u_int32_t bulk,
    bool rmw)
 
```

Constructor.

``` c
db_base_iterator(const db_base_iterator &bi)
 
```

Copy constructor. Copy all members of this class.

### Class

<a href="db_base_iterator.md" class="link" title="Chapter 10.  Db_base_iterator">db_base_iterator</a>
