---
title: "Chapter 1.  Dbstl Global Public Functions"
api-name: "Chapter 1.  Dbstl Global Public Functions"
source: docs/api_reference/STL/dbstl_global_functions.html
---
## Chapter 1.  Dbstl Global Public Functions

#### Public Members

| Member | Description |
|----|----|
| <a href="dbstl_global_functions.md#stldbstl_global_functionsclose_db" class="xref" title="close_db">close_db</a> | Close pdb regardless of reference count. |
| <a href="stldbstl_global_functionsclose_all_dbs.md" class="xref" title="close_all_dbs">close_all_dbs</a> | Close all open database handles regardless of reference count. |
| <a href="stldbstl_global_functionsclose_db_env.md" class="xref" title="close_db_env">close_db_env</a> | Close specified database environment handle regardless of reference count. |
| <a href="stldbstl_global_functionsclose_all_db_envs.md" class="xref" title="close_all_db_envs">close_all_db_envs</a> | Close all open database environment handles regardless of reference count. |
| <a href="stldbstl_global_functionsbegin_txn.md" class="xref" title="begin_txn">begin_txn</a> | Begin a new transaction from the specified environment "env". |
| <a href="stldbstl_global_functionscommit_txn.md" class="xref" title="commit_txn">commit_txn</a> | Commit current transaction opened in the environment "env". |
| <a href="stldbstl_global_functionsabort_txn.md" class="xref" title="abort_txn">abort_txn</a> | Abort current transaction of environment "env". |
| <a href="stldbstl_global_functionscurrent_txn.md" class="xref" title="current_txn">current_txn</a> | Get current transaction of environment "env". |
| <a href="stldbstl_global_functionsset_current_txn_handle.md" class="xref" title="set_current_txn_handle">set_current_txn_handle</a> | Set environment env's current transaction handle to be newtxn. |
| <a href="stldbstl_global_functionsregister_db.md" class="xref" title="register_db">register_db</a> | Register a Db handle "pdb1". |
| <a href="stldbstl_global_functionsregister_db_env.md" class="xref" title="register_db_env">register_db_env</a> | Register a DbEnv handle env1, this handle and handles opened in it will be closed by ResourceManager . |
| <a href="stldbstl_global_functionsopen_db.md" class="xref" title="open_db">open_db</a> | Helper function to open a database and register it into dbstl for the calling thread. |
| <a href="stldbstl_global_functionsopen_env.md" class="xref" title="open_env">open_env</a> | Helper function to open an environment and register it into dbstl for the calling thread. |
| <a href="stldbstl_global_functionsalloc_mutex.md" class="xref" title="alloc_mutex">alloc_mutex</a> | Allocate a Berkeley DB mutex. |
| <a href="stldbstl_global_functionslock_mutex.md" class="xref" title="lock_mutex">lock_mutex</a> | Lock a mutex, wait if it is held by another thread. |
| <a href="stldbstl_global_functionsunlock_mutex.md" class="xref" title="unlock_mutex">unlock_mutex</a> | Unlock a mutex, and return immediately. |
| <a href="stldbstl_global_functionsfree_mutex.md" class="xref" title="free_mutex">free_mutex</a> | Free a mutex, and return immediately. |
| <a href="stldbstl_global_functionsdbstl_startup.md" class="xref" title="dbstl_startup">dbstl_startup</a> | If there are multiple threads within a process that make use of dbstl, then this function should be called in a single thread mutual exclusively before any use of dbstl in a process; Otherwise, you don't need to call it, but are allowed to call it anyway. |
| <a href="stldbstl_global_functionsdbstl_exit.md" class="xref" title="dbstl_exit">dbstl_exit</a> | This function releases any memory allocated in the heap by code of dbstl. |
| <a href="stldbstl_global_functionsdbstl_thread_exit.md" class="xref" title="dbstl_thread_exit">dbstl_thread_exit</a> | This function closes all Berkeley DB handles in the right order, if other threads do not use them. |
| <a href="stldbstl_global_functionsoperator_eq.md" class="xref" title="operator==">operator==</a> | Operators to compare two Dbt objects. |
| <a href="stldbstl_global_functionsset_global_dbfile_suffix_number.md" class="xref" title="set_global_dbfile_suffix_number">set_global_dbfile_suffix_number</a> | If exisiting random temporary database name generation mechanism is still causing name clashes, users can set this global suffix number which will be append to each temporary database file name and incremented after each append, and by default it is 0. |
| <a href="stldbstl_global_functionsclose_db_cursors.md" class="xref" title="close_db_cursors">close_db_cursors</a> | Close cursors opened in dbp1. |

#### Group

None

## close_db

### Function Details

``` c
 void close_db(Db *pdb)
 
```

Close pdb regardless of reference count.

You must make sure pdb is not used by others before calling this method. You can close the underlying database of a container and assign another database with right configurations to it, if the configuration is not suitable for the container, there will be an <a href="InvalidArgumentException.md" class="link" title="Chapter 39.  InvalidArgumentException">InvalidArgumentException</a> type of exception thrown. You can't use the container after you called close_db and before setting another valid database handle to the container via <a href="stldb_containerset_db_handle.md" class="link" title="set_db_handle">db_container::set_db_handle()</a> function.

#### Parameters

##### pdb

The database handle to close.

### Group: Functions to close database/environments.

Normally you don't have to close any database or environment handles, they will be closed automatically.

Though you still have the following API to close them.

### Class

<a href="dbstl_global_functions.md" class="link" title="Chapter 1.  Dbstl Global Public Functions">dbstl_global_functions</a>
