---
title: "Secondary Indices with Transaction Applications"
api-name: "Secondary Indices with Transaction Applications"
source: docs/gsg_txn/JAVA/txnindices.html
---
## Secondary Indices with Transaction Applications

You can use transactions with your secondary indices so long as you open the secondary index so that it is transactional.

All other aspects of using secondary indices with transactions are identical to using secondary indices without transactions. In addition, transaction-protecting secondary cursors is performed just as you protect normal cursors — you simply have to make sure the cursor is opened using a transaction handle, and that the cursor is closed before the handle is either either committed or aborted. See <a href="txncursor.md" class="xref" title="Transactional Cursors">Transactional Cursors</a> for details.

Note that when you use transactions to protect your database writes, your secondary indices are protected from corruption because updates to the primary and the secondaries are performed in a single atomic transaction.

### Note

If you are using the DPL, then be aware that you never have to provide a transactional handle when opening an index, be it a primary or a secondary. However, if transactions are enabled for your store, then all of the indexes that you open will be enabled for transactional usage. Moreover, any write operation performed using that index will be done using a transaction, regardless of whether you explicitly provide a transactional handle to the write operation.

If you do not explicitly provide a transaction handle to DPL write operations performed on a transactional store, then auto commit is silently used for that operation.

For example:

``` c
package db.GettingStarted;

import com.sleepycat.bind.tuple.TupleBinding;
import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseType;
import com.sleepycat.db.DatabaseConfig;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;
import com.sleepycat.db.SecondaryDatabase;
import com.sleepycat.db.SecondaryConfig;

import java.io.FileNotFoundException;

...

// Environment and primary database opens omitted.

SecondaryConfig mySecConfig = new SecondaryConfig();
mySecConfig.setAllowCreate(true);
mySecConfig.setType(DatabaseType.BTREE);
mySecConfig.setTransactional(true);

SecondaryDatabase mySecDb = null;
try {
    // A fake tuple binding that is not actually implemented anywhere.
    // The tuple binding is dependent on the data in use.
    // See the Getting Started Guide for details
    TupleBinding myTupleBinding = new MyTupleBinding();

    // Open the secondary. FullNameKeyCreator is not actually implemented
    // anywhere. See the Getting Started Guide for details.
    FullNameKeyCreator keyCreator = 
        new FullNameKeyCreator(myTupleBinding);

    // Set the key creator on the secondary config object.
    mySecConfig.setKeyCreator(keyCreator);

    // Perform the actual open. Because this database is configured to be
    // transactional, the open is automatically wrapped in a transaction.
    //      - myEnv is the environment handle.
    //      - myDb is the primary database handle.
    String secDbName = "mySecondaryDatabase";
    mySecDb = myEnv.openSecondary(null, secDbName, null, myDb, 
                                  mySecConfig);
} catch (DatabaseException de) {
    // Exception handling goes here ...
} catch (FileNotFoundException fnfe) {
    // Exception handling goes here ...
}
```
