---
title: "Read/Modify/Write"
api-name: "Read/Modify/Write"
source: docs/gsg_txn/CXX/readmodifywrite.html
---
## Read/Modify/Write

If you are retrieving a record from the database for the purpose of modifying or deleting it, you should declare a read-modify-write cycle at the time that you read the record. Doing so causes DB to obtain write locks (instead of a read locks) at the time of the read. This helps to prevent deadlocks by preventing another transaction from acquiring a read lock on the same record while the read-modify-write cycle is in progress.

Note that declaring a read-modify-write cycle may actually increase the amount of blocking that your application sees, because readers immediately obtain write locks and write locks cannot be shared. For this reason, you should use read-modify-write cycles only if you are seeing a large amount of deadlocking occurring in your application.

In order to declare a read/modify/write cycle when you perform a read operation, pass the `DB_RMW` flag to the database or cursor get method.

For example:

``` c
// Begin the deadlock retry loop as is normal.
while (retry_count < MAX_DEADLOCK_RETRIES) {
    try {
        envp->txn_begin(NULL, txn, 0);

        ...
        // key and data are DBTs. Their usage is omitted for brevity.
        ...

        // Read the data. Declare the read/modify/write cycle here
        dbp->get(txn, &key, &data, DB_RMW);

        ...
        // Modify the data as is required. (not shown here)
        ...

        // Put the data. Note that you do not have to provide any 
        // additional flags here due to the read/modify/write 
        // cycle. Simply put the data and perform your deadlock 
        // detection as normal.
        dbp->put(txn, &key, &data, 0);
        txn->commit(0);
        return (EXIT_SUCCESS);
    } catch (DbDeadlockException &de) {
        // Deadlock detection and exception handling omitted
        // for brevity
        ...
```
