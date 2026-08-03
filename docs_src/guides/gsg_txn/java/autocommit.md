---
title: "Auto Commit"
api-name: "Auto Commit"
source: docs/gsg_txn/JAVA/autocommit.html
---
## Auto Commit

While transactions are frequently used to provide atomicity to multiple database or store operations, it is sometimes necessary to perform a single database or store operation under the control of a transaction. Rather than force you to obtain a transaction, perform the single write operation, and then either commit or abort the transaction, you can automatically group this sequence of events using <span class="emphasis">*auto commit*</span>.

To use auto commit:

1.  Open your environment and your databases or store so that they support transactions. See <a href="enabletxn.md" class="xref" title="Chapter 2. Enabling Transactions">Enabling Transactions</a> for details.

2.  Do not provide a transactional handle to the method that is performing the database or store write operation.

Note that auto commit is not available for cursors. You must always open your cursor using a transaction if you want the cursor's operations to be transactional protected. See <a href="txncursor.md" class="xref" title="Transactional Cursors">Transactional Cursors</a> for details on using transactional cursors.

### Note

Never have more than one active transaction in your thread at a time. This is especially a problem if you mix an explicit transaction with another operation that uses auto commit. Doing so can result in undetectable deadlocks.

For example, the following uses auto commit to perform the database write operation:

``` c
package db.txn;

import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseConfig;
import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;

import java.io.File;

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

    // Perform the write. Because the database was opened to 
    // support transactions, this write is performed using auto commit.
    myDatabase.put(null, key, data);

} catch (DatabaseException de) {
    // Exception handling goes here
} 
```
