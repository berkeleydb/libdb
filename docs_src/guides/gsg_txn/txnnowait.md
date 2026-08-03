---
title: "No Wait on Blocks"
api-name: "No Wait on Blocks"
source: docs/gsg_txn/C/txnnowait.html
---
## No Wait on Blocks

Normally when a DB transaction is blocked on a lock request, it must wait until the requested lock becomes available before its thread-of-control can proceed. However, it is possible to configure a transaction handle such that it will report a deadlock rather than wait for the block to clear.

You do this on a transaction by transaction basis by specifying `DB_TXN_NOWAIT` to the `DB_ENV->txn_begin()` method.

For example:

``` c
    /* Get the transaction */
    DB_TXN *txn = NULL;
    ret = envp->txn_begin(envp, NULL, &txn, DB_TXN_NOWAIT);
    if (ret != 0) {
            envp->err(envp, ret, "txn_begin failed");
            return (EXIT_FAILURE);
    }
        ....  
    /* Deadlock detection and exception handling omitted for brevity. */
```
