---
title: "Chapter 3. Transaction Basics"
api-name: "Chapter 3. Transaction Basics"
source: docs/gsg_txn/JAVA/usingtxns.html
---
## Chapter 3. Transaction Basics

**Table of Contents**

<span class="sect1"> [Committing a Transaction](usingtxns.md#commitresults) </span>

<span class="sect1"> [Non-Durable Transactions](nodurabletxn.md) </span>

<span class="sect1"> [Aborting a Transaction](abortresults.md) </span>

<span class="sect1"> [Auto Commit](autocommit.md) </span>

<span class="sect1"> [Nested Transactions](nestedtxn.md) </span>

<span class="sect1"> [Transactional Cursors](txncursor.md) </span>

<span class="sect2"> [Using Transactional DPL Cursors](txncursor.md#dplcursors) </span>

<span class="sect1"> [Secondary Indices with Transaction Applications](txnindices.md) </span>

<span class="sect1"> [Configuring the Transaction Subsystem](maxtxns.md) </span>

Once you have enabled transactions for your environment and your databases, you can use them to protect your database operations. You do this by acquiring a transaction handle and then using that handle for any database operation that you want to participate in that transaction.

You obtain a transaction handle using the `Environment.beginTransaction()` method.

Once you have completed all of the operations that you want to include in the transaction, you must commit the transaction using the `Transaction.commit()` method.

If, for any reason, you want to abandon the transaction, you abort it using `Transaction.abort()`.

Any transaction handle that has been committed or aborted can no longer be used by your application.

Finally, you must make sure that all transaction handles are either committed or aborted before closing your databases and environment.

### Note

If you only want to transaction protect a single database write operation, you can use auto commit to perform the transaction administration. When you use auto commit, you do not need an explicit transaction handle. See <a href="autocommit.md" class="xref" title="Auto Commit">Auto Commit</a> for more information.

For example, the following example opens a transactional-enabled environment and store, obtains a transaction handle, and then performs a write operation under its protection. In the event of any failure in the write operation, the transaction is aborted and the store is left in a state as if no operations had ever been attempted in the first place.

``` c
package persist.txn;

import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;
import com.sleepycat.db.Transaction;

import com.sleepycat.persist.EntityStore;
import com.sleepycat.persist.StoreConfig;

import java.io.File;
import java.io.FileNotFoundException;

...

Environment myEnv = null;
EntityStore store = null;

// Our convenience data accessor class, used for easy access to 
// EntityClass indexes.
DataAccessor da;

try {
    EnvironmentConfig myEnvConfig = new EnvironmentConfig();
    myEnvConfig.setInitializeCache(true);
    myEnvConfig.setInitializeLocking(true);
    myEnvConfig.setInitializeLogging(true);
    myEnvConfig.setTransactional(true);

    StoreConfig storeConfig = new StoreConfig();
    storeConfig.setTransactional(true);

    myEnv = new Environment(new File("/my/env/home"),
                              myEnvConfig);

    EntityStore store = new EntityStore(myEnv, 
                             "EntityStore", storeConfig);

    da = new DataAccessor(store);

    // Assume that Inventory is an entity class.
    Inventory theInventory = new Inventory();
    theInventory.setItemName("Waffles");
    theInventory.setItemSku("waf23rbni");

    Transaction txn = myEnv.beginTransaction(null, null);
        
    try {
        // Put the object to the store using the transaction handle.
        da.inventoryBySku.put(txn, theInventory);

        // Commit the transaction. The data is now safely written to the
        // store.
        txn.commit();
    // If there is a problem, abort the transaction
    } catch (Exception e) {
        if (txn != null) {
            txn.abort();
            txn = null;
        }
    }

} catch (DatabaseException de) {
    // Exception handling goes here
    } catch (FileNotFoundException fnfe) {
    // Exception handling goes here
}
```

The same thing can be done with the base API; the database in use is left unchanged if the write operation fails:

``` c
package db.txn;

import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseConfig;
import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;
import com.sleepycat.db.Transaction;

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

    myEnv = new Environment(new File("/my/env/home"),
                              myEnvConfig);

    // Open the database.
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

    Transaction txn = myEnv.beginTransaction(null, null);
        
    try {
        myDatabase.put(txn, key, data);
        txn.commit();
    } catch (Exception e) {
        if (txn != null) {
            txn.abort();
            txn = null;
        }
    }

} catch (DatabaseException de) {
    // Exception handling goes here
} catch (FileNotFoundException fnfe) {
    // Exception handling goes here
} 
```

## Committing a Transaction

In order to fully understand what is happening when you commit a transaction, you must first understand a little about what DB is doing with the logging subsystem. Logging causes all database or store write operations to be identified in logs, and by default these logs are backed by files on disk. These logs are used to restore your databases or store in the event of a system or application failure, so by performing logging, DB ensures the integrity of your data.

Moreover, DB performs <span class="emphasis">*write-ahead*</span> logging. This means that information is written to the logs <span class="emphasis">*before*</span> the actual database or store is changed. This means that all write activity performed under the protection of the transaction is noted in the log before the transaction is committed. Be aware, however, that database maintains logs in-memory. If you are backing your logs on disk, the log information will eventually be written to the log files, but while the transaction is on-going the log data may be held only in memory.

When you commit a transaction, the following occurs:

- A commit record is written to the log. This indicates that the modifications made by the transaction are now permanent. By default, this write is performed synchronously to disk so the commit record arrives in the log files before any other actions are taken.

- Any log information held in memory is (by default) synchronously written to disk. Note that this requirement can be relaxed, depending on the type of commit you perform. See <a href="nodurabletxn.md" class="xref" title="Non-Durable Transactions">Non-Durable Transactions</a> for more information. Also, if you are maintaining your logs entirely in-memory, then this step will of course not be taken. To configure your logging system for in-memory usage, see <a href="logconfig.md#inmemorylogging" class="xref" title="Configuring In-Memory Logging">Configuring In-Memory Logging</a>.

- All locks held by the transaction are released. This means that read operations performed by other transactions or threads of control can now see the modifications without resorting to uncommitted reads (see <a href="isolation.md#dirtyreads" class="xref" title="Reading Uncommitted Data">Reading Uncommitted Data</a> for more information).

To commit a transaction, you simply call `Transaction.commit()`.

Notice that committing a transaction does not necessarily cause data modified in your memory cache to be written to the files backing your databases on disk. Dirtied database pages are written for a number of reasons, but a transactional commit is not one of them. The following are the things that can cause a dirtied database page to be written to the backing database file:

- Checkpoints.

  Checkpoints cause all dirtied pages currently existing in the cache to be written to disk, and a checkpoint record is then written to the logs. You can run checkpoints explicitly. For more information on checkpoints, see <a href="filemanagement.md#checkpoints" class="xref" title="Checkpoints">Checkpoints</a>.

- Cache is full.

  If the in-memory cache fills up, then dirtied pages might be written to disk in order to free up space for other pages that your application needs to use. Note that if dirtied pages are written to the database files, then any log records that describe how those pages were dirtied are written to disk before the database pages are written.

Be aware that because your transaction commit caused database or store modifications recorded in your logs to be forced to disk, your modifications are by default "persistent" in that they can be recovered in the event of an application or system failure. However, recovery time is gated by how much data has been modified since the last checkpoint, so for applications that perform a lot of writes, you may want to run a checkpoint with some frequency.

Note that once you have committed a transaction, the transaction handle that you used for the transaction is no longer valid. To perform database activities under the control of a new transaction, you must obtain a fresh transaction handle.
