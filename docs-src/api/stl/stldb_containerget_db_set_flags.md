---
title: "get_db_set_flags"
api-name: "get_db_set_flags"
source: docs/api_reference/STL/stldb_containerget_db_set_flags.html
---
## get_db_set_flags

### Function Details

``` c
u_int32_t get_db_set_flags() const
 
```

Get the backing database's flags that are set via Db::set_flags() function.

#### Return Value

Flags set to this container's database handle.

### Group: Get and set functions for data members.

Note that these functions are not thread safe, because all data members of <a href="db_container.md" class="link" title="Chapter 3.  Db_container">db_container</a> are supposed to be set on container construction and initialization, and remain read only afterwards.

### Class

<a href="db_container.md" class="link" title="Chapter 3.  Db_container">db_container</a>
