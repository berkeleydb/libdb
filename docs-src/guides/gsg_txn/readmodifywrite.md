---
title: "Read/Modify/Write"
api-name: "Read/Modify/Write"
source: docs/gsg_txn/C/readmodifywrite.html
---
## Read/Modify/Write

If you are retrieving a record from the database for the purpose of modifying or deleting it, you should declare a read-modify-write cycle at the time that you read the record. Doing so causes DB to obtain write locks (instead of a read locks) at the time of the read. This helps to prevent deadlocks by preventing another transaction from acquiring a read lock on the same record while the read-modify-write cycle is in progress.

Note that declaring a read-modify-write cycle may actually increase the amount of blocking that your application sees, because readers immediately obtain write locks and write locks cannot be shared. For this reason, you should use read-modify-write cycles only if you are seeing a large amount of deadlocking occurring in your application.

In order to declare a read/modify/write cycle when you perform a read operation, pass the `DB_RMW` flag to the database or cursor get method.

For example:

``` c
retry:
    /* Get the transaction */
    ret = envp->txn_begin(envp, NULL, &txn, 0);
    if (ret != 0) {
            envp->err(envp, ret, "txn_begin failed");
            return (EXIT_FAILURE);
    }
    ...
    /* key and data are Dbts. Their usage is omitted for brevity. */
    ...
    /* Get the data. Declare the read/modify/write cycle here */
    ret = dbp->get(dbp, txn, &key, &data, DB_RMW);

    ...
    /* Modify the key and data as is required (not shown here) */
    ...

    /* Put the data. Note that you do not have to provide any additional
     * flags here due to the read/modify/write cycle. Simply put the 
     * data and perform your deadlock detection as normal.
     */
    ret = dbp->put(dbp, txn, &key, &data, 0);
    switch (ret) {
        /* Deadlock detection omitted for brevity */
        ....  
```
