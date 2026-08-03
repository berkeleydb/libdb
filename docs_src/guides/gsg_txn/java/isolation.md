---
title: "Isolation"
api-name: "Isolation"
source: docs/gsg_txn/JAVA/isolation.html
---
## Isolation

<span class="sect2"> [Supported Degrees of Isolation](isolation.md#degreesofisolation) </span>

<span class="sect2"> [Reading Uncommitted Data](isolation.md#dirtyreads) </span>

<span class="sect2"> [Committed Reads](isolation.md#readcommitted) </span>

<span class="sect2"> [Using Snapshot Isolation](isolation.md#snapshot_isolation) </span>

Isolation guarantees are an important aspect of transactional protection. Transactions ensure the data your transaction is working with will not be changed by some other transaction. Moreover, the modifications made by a transaction will never be viewable outside of that transaction until the changes have been committed.

That said, there are different degrees of isolation, and you can choose to relax your isolation guarantees to one degree or another depending on your application's requirements. The primary reason why you might want to do this is because of performance; the more isolation you ask your transactions to provide, the more locking that your application must do. With more locking comes a greater chance of blocking, which in turn causes your threads to pause while waiting for a lock. Therefore, by relaxing your isolation guarantees, you can <span class="emphasis">*potentially*</span> improve your application's throughput. Whether you actually see any improvement depends, of course, on the nature of your application's data and transactions.

### Supported Degrees of Isolation

DB supports the following levels of isolation:

<table data-border="1" width="80%">
<thead>
<tr>
<th>Degree</th>
<th>ANSI Term</th>
<th>Definition</th>
</tr>
</thead>
<tbody>
<tr>
<td>1</td>
<td>READ UNCOMMITTED</td>
<td>Uncommitted reads means that one transaction will never overwrite another transaction's dirty data. Dirty data is data that a transaction has modified but not yet committed to the underlying data store. However, uncommitted reads allows a transaction to see data dirtied by another transaction. In addition, a transaction may read data dirtied by another transaction, but which subsequently is aborted by that other transaction. In this latter case, the reading transaction may be reading data that never really existed in the database.</td>
</tr>
<tr>
<td>2</td>
<td>READ COMMITTED</td>
<td><p>Committed read isolation means that degree 1 is observed, except that dirty data is never read.</p>
<p>In addition, this isolation level guarantees that data will never change so long as it is addressed by the cursor, but the data may change before the reading cursor is closed. In the case of a transaction, data at the current cursor position will not change, but once the cursor moves, the previous referenced data can change. This means that readers release read locks before the cursor is closed, and therefore, before the transaction completes. Note that this level of isolation causes the cursor to operate in exactly the same way as it does in the absence of a transaction.</p></td>
</tr>
<tr>
<td>3</td>
<td>SERIALIZABLE</td>
<td><p>Committed read is observed, plus the data read by a transaction, T, will never be dirtied by another transaction before T completes. This means that both read and write locks are not released until the transaction completes.</p>
<p><span> In addition, </span> no transactions will see phantoms. Phantoms are records returned as a result of a search, but which were not seen by the same transaction when the identical search criteria was previously used.</p>
<p>This is DB's default isolation guarantee.</p></td>
</tr>
</tbody>
</table>

By default, DB transactions and transactional cursors offer serializable isolation. You can optionally reduce your isolation level by configuring DB to use uncommitted read isolation. See <a href="isolation.md#dirtyreads" class="xref" title="Reading Uncommitted Data">Reading Uncommitted Data</a> for more information. You can also configure DB to use committed read isolation. See <a href="isolation.md#readcommitted" class="xref" title="Committed Reads">Committed Reads</a> for more information.

Finally, in addition to DB's normal degrees of isolation, you can also use <span class="emphasis">*snapshot isolation*</span>. This allows you to avoid the read locks that serializable isolation requires. See <a href="isolation.md#snapshot_isolation" class="xref" title="Using Snapshot Isolation">Using Snapshot Isolation</a> for details.

### Reading Uncommitted Data

Berkeley DB allows you to configure your application to read data that has been modified but not yet committed by another transaction; that is, dirty data. When you do this, you may see a performance benefit by allowing your application to not have to block waiting for write locks. On the other hand, the data that your application is reading may change before the transaction has completed.

When used with transactions, uncommitted reads means that one transaction can see data modified but not yet committed by another transaction. When used with transactional cursors, uncommitted reads means that any database reader can see data modified by the cursor before the cursor's transaction has committed.

Because of this, uncommitted reads allow a transaction to read data that may subsequently be aborted by another transaction. In this case, the reading transaction will have read data that never really existed in the database.

To configure your application to read uncommitted data:

1.  Open your database such that it will allow uncommitted reads. You do this by specifying `true` to `DatabaseConfig.setReadUncommitted()`. (If you are using the DPL, you must provide this `DatabaseConfig` object to the entity store using the `EntityStore.setPrimaryConfig()` method.)

2.   Specify that you want to use uncommitted reads when you create a transaction or open the cursor. To do this, you use the `setReadUncommitted()` method on the relevant configuration object (`TransactionConfig` or `CursorConfig`).

For example, the following opens the database such that it supports uncommitted reads, and then creates a transaction that causes all reads performed within it to use uncommitted reads. Remember that simply opening the database to support uncommitted reads is not enough; you must also declare your read operations to be performed using uncommitted reads.

``` c
package db.txn;

import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseConfig;
import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.DatabaseType;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;
import com.sleepycat.db.Transaction;
import com.sleepycat.db.TransactionConfig;

import java.io.File;

...

Database myDatabase = null;
Environment myEnv = null;
try {
    EnvironmentConfig myEnvConfig = new EnvironmentConfig();
    myEnvConfig.setTransactional(true);
    myEnvConfig.setInitializeCache(true);
    myEnvConfig.setInitializeLocking(true);
    myEnvConfig.setInitializeLogging(true);

    myEnv = new Environment(new File("/my/env/home"),
                              myEnvConfig);

    // Open the database.
    DatabaseConfig dbConfig = new DatabaseConfig();
    dbConfig.setTransactional(true);
    dbConfig.setType(DatabaseType.BTREE);
    dbConfig.setAllowCreate(true);
    dbConfig.setReadUncommitted(true);      // Enable uncommitted reads.
    myDatabase = myEnv.openDatabase(null,              // txn handle
                                    "sampleDatabase",  // db file name
                                    null,              // db name
                                    dbConfig);
    TransactionConfig txnConfig = new TransactionConfig();
    txnConfig.setReadUncommitted(true);          // Use uncommitted reads 
                                                 // for this transaction.
    Transaction txn = myEnv.beginTransaction(null, txnConfig);

    // From here, you perform your database reads and writes as normal,
    // committing and aborting the transactions as is necessary, and
    // testing for deadlock exceptions as normal (omitted for brevity). 
        
    ...
```

If you are using the DPL:

``` c
package persist.txn;

import com.sleepycat.db.DatabaseConfig;
import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.DatabaseType;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;
import com.sleepycat.db.Transaction;
import com.sleepycat.db.TransactionConfig;

import com.sleepycat.persist.EntityStore;
import com.sleepycat.persist.StoreConfig;

import java.io.File;

...

EntityStore myStore = null;
Environment myEnv = null;
try {
    EnvironmentConfig myEnvConfig = new EnvironmentConfig();
    myEnvConfig.setTransactional(true);
    myEnvConfig.setInitializeCache(true);
    myEnvConfig.setInitializeLocking(true);
    myEnvConfig.setInitializeLogging(true);

    myEnv = new Environment(new File("/my/env/home"),
                              myEnvConfig);

    // Open the store.
    StoreConfig myStoreConfig = new StoreConfig();
    myStoreConfig.setAllowCreate(true);
    myStoreConfig.setTransactional(true);

    // You must set all these fields if you are going to use
    // a DatabaseConfig object with your new entity store.
    DatabaseConfig dbConfig = new DatabaseConfig();
    dbConfig.setTransactional(true);
    dbConfig.setAllowCreate(true);
    dbConfig.setType(DatabaseType.BTREE);
    dbConfig.setReadUncommitted(true);      // Enable uncommitted reads.

    myStore = new EntityStore(myEnv, "store_name", myStoreConfig);

    // Set the DatabaseConfig object, so that the underlying
    // database is configured for uncommitted reads.
    myStore.setPrimaryConfig(SomeEntityClass.class, dbConfig);

    TransactionConfig txnConfig = new TransactionConfig();
    txnConfig.setReadUncommitted(true);          // Use uncommitted reads 
                                                 // for this transaction.
    Transaction txn = myEnv.beginTransaction(null, txnConfig);

    // From here, you perform your store reads and writes as normal,
    // committing and aborting the transactions as is necessary, and
    // testing for deadlock exceptions as normal (omitted for brevity). 
        
    ...
```

You can also configure uncommitted read isolation on a read-by-read basis by specifying `LockMode.READ_UNCOMMITTED`:

``` c
package db.txn;

import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.Environment;
import com.sleepycat.db.LockMode;
import com.sleepycat.db.Transaction;

...

Database myDb = null;
Environment myEnv = null;
Transaction txn = null;

try {

    // Environment and database open omitted

    ...

    txn = myEnv.beginTransaction(null, null);

    DatabaseEntry theKey =
        new DatabaseEntry((new String("theKey")).getBytes("UTF-8"));
    DatabaseEntry theData = new DatabaseEntry();

    myDb.get(txn, theKey, theData, LockMode.READ_UNCOMMITTED);
} catch (Exception e) {
    // Exception handling goes here
} 
```

Using the DPL:

``` c
package persist.txn;

import com.sleepycat.db.Environment;
import com.sleepycat.db.LockMode;
import com.sleepycat.db.Transaction;

import com.sleepycat.persist.PrimaryIndex;
...

Environment myEnv = null;
Transaction txn = null;

try {

    // Environment and database open omitted

    ...

    txn = myEnv.beginTransaction(null, null);

    AnEntityClass aec = aPrimaryIndex.get(txn, "pKeya", 
                            LockMode.READ_UNCOMMITTED);
} catch (Exception e) {
    // Exception handling goes here
} 
```

### Committed Reads

You can configure your transaction so that the data being read by a transactional cursor is consistent so long as it is being addressed by the cursor. However, once the cursor is done reading the object or record (that is, reading records from the page that it currently has locked), the cursor releases its lock on that object, record or page. This means that the data the cursor has read and released may change before the cursor's transaction has completed.

For example, suppose you have two transactions, `Ta` and `Tb`. Suppose further that `Ta` has a cursor that reads `record R`, but does not modify it. Normally, `Tb` would then be unable to write `record R` because `Ta` would be holding a read lock on it. But when you configure your transaction for committed reads, `Tb` <span class="emphasis">*can*</span> modify `record R` before `Ta` completes, so long as the reading cursor is no longer addressing the object, record or page.

When you configure your application for this level of isolation, you may see better performance throughput because there are fewer read locks being held by your transactions. Read committed isolation is most useful when you have a cursor that is reading and/or writing records in a single direction, and that does not ever have to go back to re-read those same records. In this case, you can allow DB to release read locks as it goes, rather than hold them for the life of the transaction.

To configure your application to use committed reads, do one of the following:

- Create your transaction such that it allows committed reads. You do this by specifying `true` to `TransactionConfig.setReadCommitted()`.

-  Specify `true` to `CursorConfig.setReadCommitted()`.

For example, the following creates a transaction that allows committed reads:

``` c
package db.txn;

import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseConfig;
import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;
import com.sleepycat.db.Transaction;
import com.sleepycat.db.TransactionConfig;

import java.io.File;

...

Database myDatabase = null;
Environment myEnv = null;
try {
    EnvironmentConfig myEnvConfig = new EnvironmentConfig();
    myEnvConfig.setTransactional(true);
    myEnvConfig.setInitializeCache(true);
    myEnvConfig.setInitializeLocking(true);
    myEnvConfig.setInitializeLogging(true);

    myEnv = new Environment(new File("/my/env/home"),
                              myEnvConfig);

    // Open the database.
    // Notice that we do not have to specify any properties to the 
    // database to allow committed reads (this is as opposed to 
    // uncommitted reads where we DO have to specify a property on 
    // the database open.
    DatabaseConfig dbConfig = new DatabaseConfig();
    dbConfig.setTransactional(true);
    dbConfig.setType(DatabaseType.BTREE);

    myDatabase = myEnv.openDatabase(null,              // txn handle
                                    "sampleDatabase",  // db file name
                                    null,              // db name
                                    dbConfig);
    String keyString = "thekey";
    String dataString = "thedata";
    DatabaseEntry key = 
        new DatabaseEntry(keyString.getBytes("UTF-8"));
    DatabaseEntry data = 
        new DatabaseEntry(dataString.getBytes("UTF-8"));

    TransactionConfig txnConfig = new TransactionConfig();

    // Open the transaction and enable committed reads. All cursors open
    // with this transaction handle will use read committed isolation.
    txnConfig.setReadCommitted(true);
    Transaction txn = myEnv.beginTransaction(null, txnConfig);

    // From here, you perform your database reads and writes as normal,
    // committing and aborting the transactions as is necessary, and
    // testing for deadlock exceptions as normal (omitted for brevity). 

    // Using transactional cursors with concurrent applications is 
    // described in more detail in the following section.
        
    ...
```

Using the DPL:

``` c
package persist.txn;

import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;
import com.sleepycat.db.Transaction;
import com.sleepycat.db.TransactionConfig;

import com.sleepycat.persist.EntityStore;
import com.sleepycat.persist.StoreConfig;

import java.io.File;

...

EntityStore myStore = null;
Environment myEnv = null;
try {
    EnvironmentConfig myEnvConfig = new EnvironmentConfig();
    myEnvConfig.setTransactional(true);
    myEnvConfig.setInitializeCache(true);
    myEnvConfig.setInitializeLocking(true);
    myEnvConfig.setInitializeLogging(true);

    myEnv = new Environment(new File("/my/env/home"),
                              myEnvConfig);

    // Instantiate the store.
    StoreConfig myStoreConfig = new StoreConfig();
    myStoreConfig.setAllowCreate(true);
    myStoreConfig.setTransactional(true);

    TransactionConfig txnConfig = new TransactionConfig();

    // Open the transaction and enable committed reads. All cursors open
    // with this transaction handle will use read committed isolation.
    txnConfig.setReadCommitted(true);
    Transaction txn = myEnv.beginTransaction(null, txnConfig);

    // From here, you perform your store reads and writes as normal,
    // committing and aborting the transactions as is necessary, and
    // testing for deadlock exceptions as normal (omitted for brevity). 

    // Using transactional cursors with concurrent applications is 
    // described in more detail in the following section.
        
    ...
```

You can also configure read committed isolation on a read-by-read basis by specifying `LockMode.READ_COMMITTED`:

``` c
package db.txn;

import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.Environment;
import com.sleepycat.db.LockMode;
import com.sleepycat.db.Transaction;

...

Database myDb = null;
Environment myEnv = null;
Transaction txn = null;

try {

    // Environment and database open omitted

    ...

    txn = myEnv.beginTransaction(null, null);

    DatabaseEntry theKey =
        new DatabaseEntry((new String("theKey")).getBytes("UTF-8"));
    DatabaseEntry theData = new DatabaseEntry();

    myDb.get(txn, theKey, theData, LockMode.READ_COMMITTED);
} catch (Exception e) {
    // Exception handling goes here
} 
```

Using the DPL:

``` c
package persist.txn;

import com.sleepycat.db.Environment;
import com.sleepycat.db.LockMode;
import com.sleepycat.db.Transaction;

import com.sleepycat.persist.PrimaryIndex;
...

Environment myEnv = null;
Transaction txn = null;

try {

    // Environment and database open omitted

    ...

    txn = myEnv.beginTransaction(null, null);

    // Primary index creation omitted
    ...

    AnEntityClass aec = aPrimaryIndex.get(txn, "pKeya", 
                            LockMode.READ_COMMITTED);
} catch (Exception e) {
    // Exception handling goes here
} 
```

### Using Snapshot Isolation

By default DB uses serializable isolation. An important side effect of this isolation level is that read operations obtain read locks on database pages, and then hold those locks until the read operation is completed. When you are using transactional cursors, this means that read locks are held until the transaction commits or aborts. In that case, over time a transactional cursor can gradually block all other transactions from writing to the database.

You can avoid this by using snapshot isolation. Snapshot isolation uses <span class="emphasis">*multiversion concurrency control*</span> to guarantee repeatable reads. What this means is that every time a writer would take a read lock on a page, instead a copy of the page is made and the writer operates on that page copy. This frees other writers from blocking due to a read lock held on the page.

### Note

Snapshot isolation is strongly recommended for read-only threads when writer threads are also running, as this will eliminate read-write contention and greatly improve transaction throughput for your writer threads. However, in order for snapshot isolation to work for your reader-only threads, you must of course use transactions for your DB reads.

#### Snapshot Isolation Cost

Snapshot isolation does not come without a cost. Because pages are being duplicated before being operated upon, the cache will fill up faster. This means that you might need a larger cache in order to hold the entire working set in memory.

If the cache becomes full of page copies before old copies can be discarded, additional I/O will occur as pages are written to temporary "freezer" files on disk. This can substantially reduce throughput, and should be avoided if possible by configuring a large cache and keeping snapshot isolation transactions short.

You can estimate how large your cache should be by taking a checkpoint, followed by a call to the `Environment.getArchiveLogFiles()` method. The amount of cache required is approximately double the size of the remaining log files (that is, the log files that cannot be archived).

#### Snapshot Isolation Transactional Requirements

In addition to an increased cache size, you may also need to increase the number of transactions that your application supports. (See <a href="maxtxns.md" class="xref" title="Configuring the Transaction Subsystem">Configuring the Transaction Subsystem</a> for details on how to set this.) In the worst case scenario, you might need to configure your application for one more transaction for every page in the cache. This is because transactions are retained until the last page they created is evicted from the cache.

#### When to Use Snapshot Isolation

Snapshot isolation is best used when all or most of the following conditions are true:

- You can have a large cache relative to your working data set size.

- You require repeatable reads.

- You will be using transactions that routinely work on the entire database, or more commonly, there is data in your database that will be very frequently written by more than one transaction.

- Read/write contention is limiting your application's throughput, or the application is all or mostly read-only and contention for the lock manager mutex is limiting throughput.

#### How to use Snapshot Isolation

You use snapshot isolation by:

- Opening the database or store with multiversion support. You can configure this either when you open your environment or when you open your database or store. Use either the `EnvironmentConfig.setMultiversion()` or the `DatabaseConfig.setMultiversion()` option to configure this support.

- Configure your cursor or transaction to use snapshot isolation.

  To do this, specify the `TransactionConfig.setSnapshot()` option when you configure your transaction.

The simplest way to take advantage of snapshot isolation is for queries: keep update transactions using full read/write locking and use snapshot isolation on read-only transactions or cursors. This should minimize blocking of snapshot isolation transactions and will avoid deadlock errors.

If the application has update transactions which read many items and only update a small set (for example, scanning until a desired record is found, then modifying it), throughput may be improved by running some updates at snapshot isolation as well. But doing this means that you must manage deadlock errors. See <a href="lockingsubsystem.md#deadlockresolve" class="xref" title="Resolving Deadlocks">Resolving Deadlocks</a> for details.

The following code fragment turns on snapshot isolation for a transaction:

``` c
package db.txn;

import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseType;
import com.sleepycat.db.DatabaseConfig;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;

import java.io.File;
import java.io.FileNotFoundException;

...

Database myDatabase = null;
Environment myEnv = null;
try {
    EnvironmentConfig myEnvConfig = new EnvironmentConfig();
    myEnvConfig.setInitializeCache(true);
    myEnvConfig.setInitializeLocking(true);
    myEnvConfig.setInitializeLogging(true);
    myEnvConfig.setTransactional(true);
    myEnvConfig.setMultiversion(true);

    myEnv = new Environment(new File("/my/env/home"),
                              myEnvConfig);

    // Open the database.
    DatabaseConfig dbConfig = new DatabaseConfig();
    dbConfig.setTransactional(true);
    dbConfig.setType(DatabaseType.BTREE);
    myDatabase = myEnv.openDatabase(null,               // txn handle
                                    "sampleDatabase",   // db file name
                                    null,             // db name
                                    dbConfig);

...

    TransactionConfig txnConfig = new TransactionConfig();
    txnConfig.setSnapshot(true);
    txn = myEnv.beginTransaction(null, txnConfig);

...

} catch (DatabaseException de) {
    // Exception handling goes here
} catch (FileNotFoundException fnfe) {
    // Exception handling goes here
}
```

When using the DPL:

``` c
package persist.txn;

import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;

import com.sleepycat.persist.EntityStore;
import com.sleepycat.persist.StoreConfig;

import java.io.File;
import java.io.FileNotFoundException;

...

EntityStore myStore = null;
Environment myEnv = null;
try {
    EnvironmentConfig myEnvConfig = new EnvironmentConfig();
    myEnvConfig.setInitializeCache(true);
    myEnvConfig.setInitializeLocking(true);
    myEnvConfig.setInitializeLogging(true);
    myEnvConfig.setTransactional(true);
    myEnvConfig.setMultiversion(true);

    myEnv = new Environment(new File("/my/env/home"),
                              myEnvConfig);

    // Instantiate the store
    StoreConfig myStoreConfig = new StoreConfig();
    myStoreConfig.setAllowCreate(true);
    myStoreConfig.setTransactional(true);

    myStore = new EntityStore(myEnv, storeName, myStoreConfig);

...

    TransactionConfig txnConfig = new TransactionConfig();
    txnConfig.setSnapshot(true);
    txn = myEnv.beginTransaction(null, txnConfig);

...

} catch (DatabaseException de) {
    // Exception handling goes here
} catch (FileNotFoundException fnfe) {
    // Exception handling goes here
}
```
