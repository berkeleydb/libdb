---
title: "current_txn"
api-name: "current_txn"
source: docs/api_reference/STL/stldbstl_global_functionscurrent_txn.html
---
## current_txn

### Function Details

``` c
 DbTxn* current_txn(DbEnv *env)
 
```

Get current transaction of environment "env".

#### Parameters

##### env

The environment whose current transaction we want to get.

#### Return Value

Current transaction of env.

### Group: Transaction control global functions.

dbstl transaction API.

You should call these API rather than DB C/C++ API to use Berkeley DB transaction features.

### Class

<a href="dbstl_global_functions.md" class="link" title="Chapter 1.  Dbstl Global Public Functions">dbstl_global_functions</a>
