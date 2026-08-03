---
title: "Opening a Transactional Environment and Store or Database"
api-name: "Opening a Transactional Environment and Store or Database"
source: docs/gsg_txn/JAVA/envopen.html
---
## Opening a Transactional Environment and Store or Database

To enable transactions for your environment, you must initialize the transactional subsystem. Note that doing this also initializes the logging subsystem. In addition, you must initialize the memory pool (in-memory cache). You must also initialize the locking subsystem. For example, to do this with the DPL:

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

Environment myEnv = null;
EntityStore myStore = null;
try {
    EnvironmentConfig myEnvConfig = new EnvironmentConfig();
    StoreConfig storeConfig = new StoreConfig();

    myEnvConfig.setInitializeCache(true);
    myEnvConfig.setInitializeLocking(true);
    myEnvConfig.setInitializeLogging(true);
    myEnvConfig.setTransactional(true);

    storeConfig.setTransactional(true);

    myEnv = new Environment(new File("/my/env/home"),
                              myEnvConfig);

    myStore = new EntityStore(myEnv, "EntityStore", storeConfig);

} catch (DatabaseException de) {
    // Exception handling goes here
} catch (FileNotFoundException fnfe) {
     // Exception handling goes here
}
```

And when using the base API:

``` c
package db.txn;

import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;

import java.io.File;
import java.io.FileNotFoundException;

...

Environment myEnv = null;
try {
    EnvironmentConfig myEnvConfig = new EnvironmentConfig();
    myEnvConfig.setInitializeCache(true);
    myEnvConfig.setInitializeLocking(true);
    myEnvConfig.setInitializeLogging(true);
    myEnvConfig.setTransactional(true);

    myEnv = new Environment(new File("/my/env/home"),
                              myEnvConfig);

} catch (DatabaseException de) {
    // Exception handling goes here
} catch (FileNotFoundException fnfe) {
     // Exception handling goes here
}
```

You then can use the `Environment` handle to open your database(s) using `Environment.openDatabase()`. Note that when you do this, you must set `DatabaseConfig.setTransactional()` to `true`. Note that in effect this causes the database open to be transactional protected because it results in auto commit being used for the open (if a transaction is not explicitly used to protect the open). For example:

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
} catch (DatabaseException de) {
    // Exception handling goes here
} catch (FileNotFoundException fnfe) {
    // Exception handling goes here
}
```

### Note

Never close a database or store that has active transactions. Make sure all transactions are resolved (either committed or aborted) before closing the database.
