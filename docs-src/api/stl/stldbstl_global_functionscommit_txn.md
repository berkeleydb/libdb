---
title: "commit_txn"
api-name: "commit_txn"
source: docs/api_reference/STL/stldbstl_global_functionscommit_txn.html
---
## commit_txn

### Function Details

``` c
 void commit_txn(DbEnv *env,
    u_int32_t flags=0)
 
```

Commit current transaction opened in the environment "env".

This function is called by user to commit an external explicit transaction.

#### Parameters

##### flags

It is set to DbTxn::commit() funcion.

##### env

The environment whose current transaction is to be committed.

#### See Also

<a href="stldbstl_global_functionscommit_txn.md" class="link" title="commit_txn">commit_txn(DbEnv *, DbTxn *, u_int32_t)</a> ;

``` c
 void commit_txn(DbEnv *env, DbTxn *txn,
    u_int32_t flags=0)
 
```

Commit a specified transaction and all its child transactions.

#### Parameters

##### txn

The transaction to commit, can be a parent transaction of a nested transaction group, all un-aborted child transactions of it will be committed.

##### flags

It is passed to each DbTxn::commit() call.

##### env

The environment where txn is started from.

#### See Also

<a href="stldbstl_global_functionscommit_txn.md" class="link" title="commit_txn">commit_txn(DbEnv *, u_int32_t)</a> ;

### Group: Transaction control global functions.

dbstl transaction API.

You should call these API rather than DB C/C++ API to use Berkeley DB transaction features.

### Class

<a href="dbstl_global_functions.md" class="link" title="Chapter 1.  Dbstl Global Public Functions">dbstl_global_functions</a>
