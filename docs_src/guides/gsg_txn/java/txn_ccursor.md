---
title: "Transactional Cursors and Concurrent Applications"
api-name: "Transactional Cursors and Concurrent Applications"
source: docs/gsg_txn/JAVA/txn_ccursor.html
---
## Transactional Cursors and Concurrent Applications

<span class="sect2"> [Using Cursors with Uncommitted Data](txn_ccursor.md#cursordirtyreads) </span>

When you use transactional cursors with a concurrent application, remember that in the event of a deadlock you must make sure that you close your cursor before you abort and retry your transaction. This is true of both base API and DPL cursors.

Also, remember that when you are using the default isolation level, every time your cursor reads a record it locks that record until the encompassing transaction is resolved. This means that walking your database with a transactional cursor increases the chance of lock contention.

For this reason, if you must routinely walk your database with a transactional cursor, consider using a reduced isolation level such as read committed. This is true of both base API and DPL cursors.

### Using Cursors with Uncommitted Data

As described in <a href="isolation.md#dirtyreads" class="xref" title="Reading Uncommitted Data">Reading Uncommitted Data</a> above, it is possible to relax your transaction's isolation level such that it can read data modified but not yet committed by another transaction. You can configure this when you create your transaction handle, and when you do so then all cursors opened inside that transaction will automatically use uncommitted reads.

You can also do this when you create a cursor handle from within a serializable transaction. When you do this, only those cursors configured for uncommitted reads uses uncommitted reads.

Either way, you must first configure your database or store handle to support uncommitted reads before you can configure your transactions or your cursors to use them.

The following example shows how to configure an individual cursor handle to read uncommitted data from within a serializable (full isolation) transaction. For an example of configuring a transaction to perform uncommitted reads in general, see <a href="isolation.md#dirtyreads" class="xref" title="Reading Uncommitted Data">Reading Uncommitted Data</a>.

``` c
package db.txn;

import com.sleepycat.db.Cursor;
import com.sleepycat.db.CursorConfig;
import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseConfig;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;

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
    dbConfig.setReadUncommitted(true);      // Enable uncommitted reads.
    myDatabase = myEnv.openDatabase(null,              // txn handle
                                    "sampleDatabase",  // db file name
                                    null,              // db name
                                    dbConfig);

    // Open the transaction. Note that this is a degree 3
    // transaction.
    Transaction txn = myEnv.beginTransaction(null, null);
    Cursor cursor = null;
    try {
        // Use the transaction handle here
        // Get our cursor. Note that we pass the transaction 
        // handle here. Note also that we cause the cursor 
        // to perform uncommitted reads.
        CursorConfig cconfig = new CursorConfig();
        cconfig.setReadUncommitted(true);
        cursor = db.openCursor(txn, cconfig);

        // From here, you perform your cursor reads and writes 
        // as normal, committing and aborting the transactions as 
        // is necessary, and testing for deadlock exceptions as 
        // normal (omitted for brevity). 
        
        ... 
```

If you are using the DPL:

``` c
package persist.txn;

import com.sleepycat.db.CursorConfig;
import com.sleepycat.db.DatabaseConfig;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;

import com.sleepycat.persist.EntityCursor;
import com.sleepycat.persist.EntityStore;
import com.sleepycat.persist.PrimaryIndex;
import com.sleepycat.persist.StoreConfig;

import java.util.Iterator;

import java.io.File;

...

EntityStore myStore = null;
Environment myEnv = null;
PrimaryIndex<String,AnEntityClass> pKey;

try {

    EnvironmentConfig myEnvConfig = new EnvironmentConfig();
    myEnvConfig.setTransactional(true);
    myEnvConfig.setInitializeCache(true);
    myEnvConfig.setInitializeLocking(true);
    myEnvConfig.setInitializeLogging(true);

    myEnv = new Environment(new File("/my/env/home"),
                              myEnvConfig);

    // Set up the entity store
    StoreConfig myStoreConfig = new StoreConfig();
    myStoreConfig.setAllowCreate(true);
    myStoreConfig.setTransactional(true);

    // Configure uncommitted reads
    DatabaseConfig dbConfig = new DatabaseConfig();
    dbConfig.setTransactional(true);
    dbConfig.setType(DatabaseType.BTREE);
    dbConfig.setAllowCreate(true);
    dbConfig.setReadUncommitted(true);      // Enable uncommitted reads.

    // Instantiate the store
    myStore = new EntityStore(myEnv, storeName, myStoreConfig);

    // Set the DatabaseConfig object, so that the underlying
    // database is configured for uncommitted reads.
    myStore.setPrimaryConfig(AnEntityClass.class, myDbConfig);

    // Open the transaction. Note that this is a degree 3
    // transaction.
    Transaction txn = myEnv.beginTransaction(null, null);

    //Configure our cursor for uncommitted reads.
    CursorConfig cconfig = new CursorConfig();
    cconfig.setReadUncommitted(true);

    // Get our cursor. Note that we pass the transaction 
    // handle here. Note also that we cause the cursor 
    // to perform uncommitted reads.
    EntityCursor<AnEntityClass> cursor = pKey.entities(txn, cconfig);

    try {
        // From here, you perform your cursor reads and writes 
        // as normal, committing and aborting the transactions as 
        // is necessary, and testing for deadlock exceptions as 
        // normal (omitted for brevity). 
        
        ... 
```
