---
title: "set_commit_flags"
api-name: "set_commit_flags"
source: docs/api_reference/STL/stldb_containerset_commit_flags.html
---
## set_commit_flags

### Function Details

``` c
void set_commit_flags(u_int32_t flag)
 
```

Set flag of DbTxn::commit() call.

#### Parameters

##### flag

Flags to be set to DbTxn::commit().

### Group: Get and set functions for data members.

Note that these functions are not thread safe, because all data members of <a href="db_container.md" class="link" title="Chapter 3.  Db_container">db_container</a> are supposed to be set on container construction and initialization, and remain read only afterwards.

### Class

<a href="db_container.md" class="link" title="Chapter 3.  Db_container">db_container</a>
