---
title: "set_all_flags"
api-name: "set_all_flags"
source: docs/api_reference/STL/stldb_containerset_all_flags.html
---
## set_all_flags

### Function Details

``` c
void set_all_flags(u_int32_t txn_begin_flags, u_int32_t commit_flags,
    u_int32_t cursor_open_flags)
 
```

Set the flags required by the Berkeley DB functions DbEnv::txn_begin(), DbTxn::commit() and DbEnv::cursor().

These flags will be set to this container's auto commit member functions when auto commit transaction is used, except that cursor_oflags is set to the Dbc::cursor when creating an iterator for this container. By default the three flags are all zero. You can also set the values of the flags individually by using the appropriate set functions in this class. The corresponding get functions return the flags actually used.

#### Parameters

##### commit_flags

Flags to be set to DbTxn::commit().

##### cursor_open_flags

Flags to be set to Db::cursor().

##### txn_begin_flags

Flags to be set to DbEnv::txn_begin().

### Group: Get and set functions for data members.

Note that these functions are not thread safe, because all data members of <a href="db_container.md" class="link" title="Chapter 3.  Db_container">db_container</a> are supposed to be set on container construction and initialization, and remain read only afterwards.

### Class

<a href="db_container.md" class="link" title="Chapter 3.  Db_container">db_container</a>
