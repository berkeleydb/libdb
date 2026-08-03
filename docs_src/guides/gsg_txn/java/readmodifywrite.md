---
title: "Read/Modify/Write"
api-name: "Read/Modify/Write"
source: docs/gsg_txn/JAVA/readmodifywrite.html
---
## Read/Modify/Write

If you are retrieving a record from the database or a class from the store for the purpose of modifying or deleting it, you should declare a read-modify-write cycle at the time that you read the record. Doing so causes DB to obtain write locks (instead of a read locks) at the time of the read. This helps to prevent deadlocks by preventing another transaction from acquiring a read lock on the same record while the read-modify-write cycle is in progress.

Note that declaring a read-modify-write cycle may actually increase the amount of blocking that your application sees, because readers immediately obtain write locks and write locks cannot be shared. For this reason, you should use read-modify-write cycles only if you are seeing a large amount of deadlocking occurring in your application.

In order to declare a read/modify/write cycle when you perform a read operation, specify `com.sleepycat.db.LockMode.RMW` to the database, cursor, `PrimaryIndex`, or `SecondaryIndex` get method.

For example:

``` c
// Begin the deadlock retry loop as is normal.
while (retry_count < MAX_DEADLOCK_RETRIES) {
    try {
        txn = myEnv.beginTransaction(null, null);

        ...
        // key and data are DatabaseEntry objects.
        // Their usage is omitted for brevity.
        ...

        // Read the data. Declare the read/modify/write cycle here
        myDatabase.get(txn, key, data, LockMode.RMW);

        // Put the data. Note that you do not have to provide any 
        // additional flags here due to the read/modify/write 
        // cycle. Simply put the data and perform your deadlock 
        // detection as normal.
        myDatabase.put(txn, key, data);
        txn.commit();
        return 0;
    } catch (DeadlockException de) {
        // Deadlock detection and exception handling omitted
        // for brevity
        ... 
```

Or, with the DPL:

``` c
// Begin the deadlock retry loop as is normal
        while (retry_count < MAX_DEADLOCK_RETRIES) {
    try {
        txn = myEnv.beginTransaction(null, null);

        ...
        // 'store' is an EntityStore and 'Inventory' is an entity class
        // Their usage and implementation is omitted for brevity.
        ...

        // Read the data, using the PrimaryIndex for the entity object
        PrimaryIndex<String,Inventory> pi = 
                store.getPrimaryIndex(String.class, Inventory.class);
        Inventory iv = pi.get(txn, "somekey", LockMode.RMW);

        // Do something to the retreived object

        // Put the object. Note that you do not have to provide any 
        // additional flags here due to the read/modify/write 
        // cycle. Simply put the data and perform your deadlock 
        // detection as normal.

        pi.put(txn, iv);
        txn.commit();
        return 0;

    } catch (DeadlockException de) {
        // Deadlock detection and exception handling omitted
        // for brevity
        ... 
```
