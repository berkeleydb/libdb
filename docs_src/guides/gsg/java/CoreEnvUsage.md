---
title: "Managing Databases in Environments"
api-name: "Managing Databases in Environments"
source: docs/gsg/JAVA/CoreEnvUsage.html
---
## Managing Databases in Environments

In <a href="Env.md" class="xref" title="Chapter 2. Database Environments">Database Environments</a>, we introduced environments. While environments are not used in the example built in this book, they are so commonly used for a wide class of DB applications that it is necessary to show their basic usage, if only from a completeness perspective.

To use an environment, you must first open it. At open time, you must identify the directory in which it resides. This directory must exist prior to the open attempt. You can also identify open properties, such as whether the environment can be created if it does not already exist.

You will also need to initialize the in-memory cache when you open your environment.

For example, to create an environment handle and open an environment:

``` c
package db.GettingStarted;

import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;

import java.io.File;
import java.io.FileNotFoundException;

...

Environment myEnv = null;
File envHome = new File("/export1/testEnv");
try {
    EnvironmentConfig envConf = new EnvironmentConfig();
    envConf.setAllowCreate(true);         // If the environment does not
                                          // exist, create it.
    envConf.setInitializeCache(true);     // Initialize the in-memory
                                          // cache.

    myEnv = new Environment(envHome, envConf);
} catch (DatabaseException de) {
    // Exception handling goes here
} catch (FileNotFoundException fnfe) {
    // Exception handling goes here
} 
```

Once an environment is opened, you can open databases in it. Note that by default databases are stored in the environment's home directory, or relative to that directory if you provide any sort of a path in the database's file name:

``` c
package db.GettingStarted;

import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseConfig;
import com.sleepycat.db.DatabaseType;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;

import java.io.File;
import java.io.FileNotFoundException;

...

Environment myEnv = null;
Database myDb = null;
File envHome = new File("/export1/testEnv");
String dbFileName = new String("mydb.db".getBytes("UTF-8"), "UTF-8");

try {
    EnvironmentConfig envConf = new EnvironmentConfig();
    envConf.setAllowCreate(true);
    DatabaseConfig dbConfig = new DatabaseConfig();
    dbConfig.setAllowCreate(true);
    dbConfig.setType(DatabaseType.BTREE);

    myEnv = new Environment(envHome, envConf);
    myDb = myEnv.openDatabase(null, dbFileName, null, dbConfig);
} catch (DatabaseException de) {
    // Exception handling goes here
} catch (FileNotFoundException fnfe) {
    // Exception handling goes here
} 
```

When you are done with an environment, you must close it. It is recommended that before closing an environment, you close any open databases.

``` c
finally {
    try {
        if (myDb != null) {
            myDb.close();
        }

        if (myEnv != null) {
            myEnv.close();
        }
    } catch (DatabaseException de) {
        // Exception handling goes here
    }
} 
```
