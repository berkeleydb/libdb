---
title: "abort_txn"
api-name: "abort_txn"
source: docs/api_reference/STL/stldbstl_global_functionsabort_txn.html
---
## abort_txn

### Function Details

``` c
 void abort_txn(DbEnv *env)
 
```

Abort current transaction of environment "env".

This function is called by dbstl user to abort an outside explicit transaction.

#### Parameters

##### env

The environment whose current transaction is to be aborted.

#### See Also

<a href="stldbstl_global_functionsabort_txn.md" class="link" title="abort_txn">abort_txn(DbEnv *, DbTxn *)</a> ;

``` c
 void abort_txn(DbEnv *env,
    DbTxn *txn)
 
```

Abort specified transaction "txn" and all its child transactions.

That is, "txn" can be a parent transaction of a nested transaction group.

#### Parameters

##### txn

The transaction to abort, can be a parent transaction of a nested transaction group, all child transactions of it will be aborted.

##### env

The environment where txn is started from.

#### See Also

<a href="stldbstl_global_functionsabort_txn.md" class="link" title="abort_txn">abort_txn(DbEnv *)</a> ;

### Group: Transaction control global functions.

dbstl transaction API.

You should call these API rather than DB C/C++ API to use Berkeley DB transaction features.

### Class

<a href="dbstl_global_functions.md" class="link" title="Chapter 1.  Dbstl Global Public Functions">dbstl_global_functions</a>
