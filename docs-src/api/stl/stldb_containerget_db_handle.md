---
title: "get_db_handle"
api-name: "get_db_handle"
source: docs/api_reference/STL/stldb_containerget_db_handle.html
---
## get_db_handle

### Function Details

``` c
Db* get_db_handle() const
 
```

Get the backing database's handle.

#### Return Value

The backing database handle of this container.

### Group: Get and set functions for data members.

Note that these functions are not thread safe, because all data members of <a href="db_container.md" class="link" title="Chapter 3.  Db_container">db_container</a> are supposed to be set on container construction and initialization, and remain read only afterwards.

### Class

<a href="db_container.md" class="link" title="Chapter 3.  Db_container">db_container</a>
