---
title: "Chapter 3.  Db_container"
api-name: "Chapter 3.  Db_container"
source: docs/api_reference/STL/db_container.html
---
## Chapter 3.  Db_container

This class is the base class for all db container classes, you don't directly use this class, but all container classes inherit from this class, so you need to know the methods that can be accessed via concrete container classes.

This class is also used to support auto commit transactions. Autocommit is enabled when DB_AUTO_COMMIT is set to the database or database environment handle and the environment is transactional.

Inside dbstl, there are transactions begun and committed/aborted if the backing database and/or environment requires auto commit, and there are cursors opened internally, and you can set the flags used by the transaction and cursor functions via set functions of this class.

All dbstl containers are fully multi-threaded, you should not need any synchronization to use them in the correct way, but this class is not thread safe, access to its members are not proctected by any mutex because the data members of this class are supposed to be set before they are used, and remain read only afterwards. If this is not the case, you must synchronize the access.

#### Public Members

| Member | Description |
|----|----|
| <a href="db_container.md#stldb_containerget_db_open_flags" class="xref" title="get_db_open_flags">get_db_open_flags</a> | Get the backing database's open flags. |
| <a href="stldb_containerget_db_set_flags.md" class="xref" title="get_db_set_flags">get_db_set_flags</a> | Get the backing database's flags that are set via Db::set_flags() function. |
| <a href="stldb_containerget_db_handle.md" class="xref" title="get_db_handle">get_db_handle</a> | Get the backing database's handle. |
| <a href="stldb_containerget_db_env_handle.md" class="xref" title="get_db_env_handle">get_db_env_handle</a> | Get the backing database environment's handle. |
| <a href="stldb_containerset_db_handle.md" class="xref" title="set_db_handle">set_db_handle</a> | Set the underlying database's handle, and optionally environment handle if the environment has also changed. |
| <a href="stldb_containerset_all_flags.md" class="xref" title="set_all_flags">set_all_flags</a> | Set the flags required by the Berkeley DB functions DbEnv::txn_begin(), DbTxn::commit() and DbEnv::cursor(). |
| <a href="stldb_containerset_txn_begin_flags.md" class="xref" title="set_txn_begin_flags">set_txn_begin_flags</a> | Set flag of DbEnv::txn_begin() call. |
| <a href="stldb_containerget_txn_begin_flags.md" class="xref" title="get_txn_begin_flags">get_txn_begin_flags</a> | Get flag of DbEnv::txn_begin() call. |
| <a href="stldb_containerset_commit_flags.md" class="xref" title="set_commit_flags">set_commit_flags</a> | Set flag of DbTxn::commit() call. |
| <a href="stldb_containerget_commit_flags.md" class="xref" title="get_commit_flags">get_commit_flags</a> | Get flag of DbTxn::commit() call. |
| <a href="stldb_containerget_cursor_open_flags.md" class="xref" title="get_cursor_open_flags">get_cursor_open_flags</a> | Get flag of Db::cursor() call. |
| <a href="stldb_containerset_cursor_open_flags.md" class="xref" title="set_cursor_open_flags">set_cursor_open_flags</a> | Set flag of Db::cursor() call. |
| <a href="stldb_containerdb_container.md" class="xref" title="db_container">db_container</a> | Default constructor. |
| <a href="stldb_containerdstr_db_container.md" class="xref" title="~db_container">~db_container</a> | The backing database is not closed in this function. |

#### Group

<a href="dbstl_containers.md" class="xref" title="Chapter 2.  Dbstl Container Classes">Dbstl Container Classes</a>

## get_db_open_flags

### Function Details

``` c
u_int32_t get_db_open_flags() const
 
```

Get the backing database's open flags.

#### Return Value

The backing database's open flags.

### Group: Get and set functions for data members.

Note that these functions are not thread safe, because all data members of <a href="db_container.md" class="link" title="Chapter 3.  Db_container">db_container</a> are supposed to be set on container construction and initialization, and remain read only afterwards.

### Class

<a href="db_container.md" class="link" title="Chapter 3.  Db_container">db_container</a>
