---
title: "Environment Properties"
api-name: "Environment Properties"
source: docs/gsg/JAVA/EnvProps.html
---
## Environment Properties

<span class="sect2"> [The EnvironmentConfig Class](EnvProps.md#envconfig) </span>

<span class="sect2"> [EnvironmentMutableConfig](EnvProps.md#envhandleconfig) </span>

You set properties for the `Environment` using the `EnvironmentConfig` class. You can also set properties for a specific `Environment` instance using `EnvironmentMutableConfig`.

### The EnvironmentConfig Class

The `EnvironmentConfig` class makes a large number of fields and methods available to you. Describing all of these tuning parameters is beyond the scope of this manual. However, there are a few properties that you are likely to want to set. They are described here.

Note that for each of the properties that you can commonly set, there is a corresponding getter method. Also, you can always retrieve the `EnvironmentConfig` object used by your environment using the `Environment.getConfig()` method.

You set environment configuration parameters using the following methods on the `EnvironmentConfig` class:

- `EnvironmentConfig.setAllowCreate()`

  If `true`, the database environment is created when it is opened. If `false`, environment open fails if the environment does not exist. This property has no meaning if the database environment already exists. Default is `false`.

- `EnvironmentConfig.setReadOnly()`

  If `true`, then all databases opened in this environment must be opened as read-only. If you are writing a multi-process application, then all but one of your processes must set this value to `true`. Default is `false`.

- `EnvironmentConfig.setTransactional()`

  If `true`, configures the database environment to support transactions. Default is `false`.

For example:

``` c
package db.gettingStarted;

import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;

import java.io.File;
import java.io.FileNotFoundException;
     
...

Environment myDatabaseEnvironment = null;
try {
    EnvironmentConfig envConfig = new EnvironmentConfig();
    envConfig.setAllowCreate(true);
    envConfig.setTransactional(true);
    myDatabaseEnvironment = 
        new Environment(new File("/export/dbEnv"), envConfig);
} catch (DatabaseException dbe) {
   System.err.println(dbe.toString());
   System.exit(1);
} catch (FileNotFoundException fnfe) {
    System.err.println(fnfe.toString());
    System.exit(-1);
} 
```

### EnvironmentMutableConfig

`EnvironmentMutableConfig` manages properties that can be reset after the `Environment` object has been constructed. In addition, `EnvironmentConfig` extends `EnvironmentMutableConfig`, so you can set these mutable properties at `Environment` construction time if necessary.

The `EnvironmentMutableConfig` class allows you to set the following properties:

- `setCachePercent()`

  Determines the percentage of JVM memory available to the DB cache. See <a href="cachesize.md" class="xref" title="Selecting the Cache Size">Selecting the Cache Size</a> for more information.

- `setCacheSize()`

  Determines the total amount of memory available to the database cache. See <a href="cachesize.md" class="xref" title="Selecting the Cache Size">Selecting the Cache Size</a> for more information.

- `setTxnNoSync()`

  Determines whether change records created due to a transaction commit are written to the backing log files on disk. A value of `true` causes the data to not be flushed to disk. See the *Getting Started with Transaction Processing for Java* guide for more information.

- `setTxnWriteNoSync()`

  Determines whether logs are flushed on transaction commit (the logs are still written, however). By setting this value to `true`, you potentially gain better performance than if you flush the logs on commit, but you do so by losing some of your transaction durability guarantees. See the *Getting Started with Transaction Processing for Java* guide for more information.

There is also a corresponding getter method (`getTxnNoSync()`). Moreover, you can always retrieve your environment's `EnvironmentMutableConfig` object by using the `Environment.getMutableConfig()` method.

For example:

``` c
package db.gettingStarted;

import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentMutableConfig;

import java.io.File;
import java.io.FileNotFoundException;

...

try {
    Environment myEnv = new Environment(new File("/export/dbEnv"), null);
    EnvironmentMutableConfig envMutableConfig = 
        new EnvironmentMutableConfig();
    envMutableConfig.setTxnNoSync(true);
    myEnv.setMutableConfig(envMutableConfig); 
} catch (DatabaseException dbe) {
    // Exception handling goes here
} catch (FileNotFoundException fnfe) {
    // Exception handling goes here
}
```
