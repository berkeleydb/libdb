---
title: "No Wait on Blocks"
api-name: "No Wait on Blocks"
source: docs/gsg_txn/JAVA/txnnowait.html
---
## No Wait on Blocks

Normally when a DB transaction is blocked on a lock request, it must wait until the requested lock becomes available before its thread-of-control can proceed. However, it is possible to configure a transaction handle such that it will report a deadlock rather than wait for the block to clear.

You do this on a transaction by transaction basis by specifying `true` to the `TransactionConfig.setNoWait()` method.

For example:

``` c
    Transaction txn = null;
    try {
        TransactionConfig tc = new TransactionConfig();
        tc.setNoWait(true);
        txn = myEnv.beginTransaction(null, tc);

        ...
    } catch (DatabaseException de) {
        // Deadlock detection and exception handling omitted
        // for brevity
        ... 
```
