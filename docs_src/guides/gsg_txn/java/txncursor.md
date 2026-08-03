---
title: "Transactional Cursors"
api-name: "Transactional Cursors"
source: docs/gsg_txn/JAVA/txncursor.html
---
## Transactional Cursors

<span class="sect2"> [Using Transactional DPL Cursors](txncursor.md#dplcursors) </span>

You can transaction-protect your cursor operations by specifying a transaction handle at the time that you create your cursor. Beyond that, you do not ever provide a transaction handle directly to a cursor method.

Note that if you transaction-protect a cursor, then you must make sure that the cursor is closed before you either commit or abort the transaction. For example:

``` c
package db.txn;

import com.sleepycat.db.Cursor;
import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseConfig;
import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;
import com.sleepycat.db.LockMode;
import com.sleepycat.db.OperationStatus;
import com.sleepycat.db.Transaction;

import java.io.File;
import java.io.FileNotFoundException;

...

Database myDatabase = null;
Environment myEnv = null;
try {

    // Database and environment opens omitted

    String replacementData = "new data";

    Transaction txn = myEnv.beginTransaction(null, null);
    Cursor cursor = null;
    try {
        // Use the transaction handle here
        cursor = db.openCursor(txn, null);
        DatabaseEntry key, data;
        
        DatabaseEntry key, data;
        while(cursor.getNext(key, data, LockMode.DEFAULT) ==
           OperationStatus.SUCCESS) {
            
            data.setData(replacementData.getBytes("UTF-8"));
            // No transaction handle is used on the cursor read or write
            // methods.
            cursor.putCurrent(data);
        }
        
        cursor.close();
        cursor = null;
        txn.commit();
        txn = null;
    } catch (Exception e) {
        if (cursor != null) {
            cursor.close();
        }
        if (txn != null) {
            txn.abort();
            txn = null;
        }
    }

} catch (DatabaseException de) {
    // Exception handling goes here
} 
```

### Using Transactional DPL Cursors

When using the DPL, you create the cursor using the entity class's primary or secondary index (see the *Getting Started with Berkeley DB for Java* guide for details). At the time that you create the cursor, you pass a transaction handle to the `entities()` method, and this causes all subsequent operations performed using that cursor to be performed within the scope of the transaction.

Note that if you are using a transaction-enabled store, then you must provide a transaction handle when you open your cursor.

For example:

``` c
package persist.txn;

import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;
import com.sleepycat.db.Transaction;

import com.sleepycat.persist.EntityCursor;
import com.sleepycat.persist.EntityStore;
import com.sleepycat.persist.PrimaryIndex;

import java.io.File;
import java.io.FileNotFoundException;

...

Environment myEnv = null;
EntityStore store = null;

...

    // Store and environment open omitted, as is the DataAccessor
    // instantiation.

...

    Transaction txn = myEnv.beginTransaction(null, null);
    PrimaryIndex<String,Inventory> pi =
        store.getPrimaryIndex(String.class, Inventory.class);
    EntityCursor<Inventory> pi_cursor = pi.entities(txn, null);

    try {
        for (Inventory ii : pi_cursor) {
            // do something with each object "ii"
            // A transactional handle is not required for any write
            // operations. All operations performed using this cursor
            // will be done within the scope of the transaction, txn.
        }
        pi_cursor.close();
        pi_cursor = null;
        txn.commit();
        txn = null;
    // Always make sure the cursor is closed when we are done with it.
    } catch (Exception e) {
        if (pi_cursor != null) {
            pi_cursor.close();
        }
        if (txn != null) {
            txn.abort();
            txn = null;
        }
    } 
```
