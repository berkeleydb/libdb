---
title: "begin_txn"
api-name: "begin_txn"
source: docs/api_reference/STL/stldbstl_global_functionsbegin_txn.html
---
## begin_txn

### Function Details

``` c
 DbTxn* begin_txn(u_int32_t flags,
    DbEnv *env)
 
```

Begin a new transaction from the specified environment "env".

This function is called by dbstl user to begin an external transaction. The "flags" parameter is passed to DbEnv::txn_begin(). If a transaction created from the same database environment already exists and is unresolved, the new transaction is started as a child transaction of that transaction, and thus you can't specify the parent transaction.

#### Parameters

##### flags

It is set to DbEnv::txn_begin() function.

##### env

The environment to start a transaction from.

#### Return Value

The newly created transaction.

### Group: Transaction control global functions.

dbstl transaction API.

You should call these API rather than DB C/C++ API to use Berkeley DB transaction features.

### Class

<a href="dbstl_global_functions.md" class="link" title="Chapter 1.  Dbstl Global Public Functions">dbstl_global_functions</a>
