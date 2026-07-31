---
title: "set_current_txn_handle"
api-name: "set_current_txn_handle"
source: docs/api_reference/STL/stldbstl_global_functionsset_current_txn_handle.html
---
## set_current_txn_handle

### Function Details

``` c
 DbTxn* set_current_txn_handle(DbEnv *env,
    DbTxn *newtxn)
 
```

Set environment env's current transaction handle to be newtxn.

The original transaction handle returned without aborting or commiting. This function is used for users to use one transaction among multiple threads.

#### Parameters

##### newtxn

The new transaction to be as the current transaction of env.

##### env

The environment whose current transaction to replace.

#### Return Value

The old current transaction of env. It is not resolved.

### Group: Transaction control global functions.

dbstl transaction API.

You should call these API rather than DB C/C++ API to use Berkeley DB transaction features.

### Class

<a href="dbstl_global_functions.md" class="link" title="Chapter 1.  Dbstl Global Public Functions">dbstl_global_functions</a>
