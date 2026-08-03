---
title: "Configuring the Transaction Subsystem"
api-name: "Configuring the Transaction Subsystem"
source: docs/gsg_txn/JAVA/maxtxns.html
---
## Configuring the Transaction Subsystem

Most of the configuration activities that you need to perform for your transactional DB application will involve the locking and logging subsystems. See <a href="txnconcurrency.md" class="xref" title="Chapter 4. Concurrency">Concurrency</a> and <a href="filemanagement.md" class="xref" title="Chapter 5. Managing DB Files">Managing DB Files</a> for details.

However, there are a couple of things that you can do to configure your transaction subsystem directly. These things are:

- 

  Configure the maximum number of simultaneous transactions needed by your application. In general, you should not need to do this unless you use deeply nested transactions or you have many threads all of which have active transactions. In addition, you may need to configure a higher maximum number of transactions if you are using snapshot isolation. See <a href="isolation.md#sisolation_maxtxn" class="xref" title="Snapshot Isolation Transactional Requirements">Snapshot Isolation Transactional Requirements</a> for details.

  By default, your application can support 20 active transactions.

  You can set the maximum number of simultaneous transactions supported by your application using `EnvironmentConfig.setTxnMaxActive()`.

  If your application has exceeded this maximum value, then any attempt to begin a new transaction will fail.

  This value can also be set using the `DB_CONFIG` file's `set_tx_max` parameter. Remember that the `DB_CONFIG` must reside in your environment home directory.

- 

  Configure the timeout value for your transactions. This value represents the longest period of time a transaction can be active. Note, however, that transaction timeouts are checked only when DB examines its lock tables for blocked locks (see <a href="blocking_deadlocks.md" class="xref" title="Locks, Blocks, and Deadlocks">Locks, Blocks, and Deadlocks</a> for more information). Therefore, a transaction's timeout can have expired, but the application will not be notified until DB has a reason to examine its lock tables.

  Be aware that some transactions may be inappropriately timed out before the transaction has a chance to complete. You should therefore use this mechanism only if you know your application might have unacceptably long transactions and you want to make sure your application will not stall during their execution. (This might happen if, for example, your transaction blocks or requests too much data.)

  Note that by default transaction timeouts are set to 0 seconds, which means that they never time out.

  To set the maximum timeout value for your transactions, use the `EnvironmentConfig.setTxnTimeout()` method. This method configures the entire environment; not just the handle used to set the configuration. Further, this value may be set at any time during the application's lifetime. (Use `Environment.setConfig()` to set this value after the environment has been opened.)

  This value can also be set using the `DB_CONFIG` file's `set_txn_timeout` parameter.

For example:

``` c
package db.txn;

import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;
import com.sleepycat.db.LockDetectMode;

import java.io.File;
import java.io.FileNotFoundException;

...

Environment myEnv = null;
try {
    EnvironmentConfig myEnvConfig = new EnvironmentConfig();
    myEnvConfig.setTransactional(true);
    myEnvConfig.setInitializeCache(true);
    myEnvConfig.setInitializeLocking(true);
    myEnvConfig.setInitializeLogging(true);

    // Configure a maximum transaction timeout of 1 second.
    myEnvConfig.setTxnTimeout(1000000);
    // Configure 40 maximum transactions.
    myEnv.setTxnMaxActive(40);

    myEnv = new Environment(new File("/my/env/home"),
                              myEnvConfig);

    // From here, you open your databases (or store), proceed with your 
    // database or store operations, and respond to deadlocks as is 
    // normal (omitted for brevity).

    ...
```
