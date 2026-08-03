---
title: "get_cursor_open_flags"
api-name: "get_cursor_open_flags"
source: docs/api_reference/STL/stldb_containerget_cursor_open_flags.html
---
## get_cursor_open_flags

### Function Details

``` c
u_int32_t get_cursor_open_flags() const
 
```

Get flag of Db::cursor() call.

#### Return Value

Flags to be set to Db::cursor().

### Group: Get and set functions for data members.

Note that these functions are not thread safe, because all data members of <a href="db_container.md" class="link" title="Chapter 3.  Db_container">db_container</a> are supposed to be set on container construction and initialization, and remain read only afterwards.

### Class

<a href="db_container.md" class="link" title="Chapter 3.  Db_container">db_container</a>
