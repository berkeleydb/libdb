---
title: "Configuring the Logging Subsystem"
api-name: "Configuring the Logging Subsystem"
source: docs/gsg_txn/JAVA/logconfig.html
---
## Configuring the Logging Subsystem

<span class="sect2"> [Setting the Log File Size](logconfig.md#logfilesize) </span>

<span class="sect2"> [Configuring the Logging Region Size](logconfig.md#logregionsize) </span>

<span class="sect2"> [Configuring In-Memory Logging](logconfig.md#inmemorylogging) </span>

<span class="sect2"> [Setting the In-Memory Log Buffer Size](logconfig.md#logbuffer) </span>

You can configure the following aspects of the logging subsystem:

- Size of the log files.

- Size of the logging subsystem's region. See <a href="logconfig.md#logregionsize" class="xref" title="Configuring the Logging Region Size">Configuring the Logging Region Size</a>.

- Maintain logs entirely in-memory. See <a href="logconfig.md#inmemorylogging" class="xref" title="Configuring In-Memory Logging">Configuring In-Memory Logging</a> for more information.

- Size of the log buffer in memory. See <a href="logconfig.md#logbuffer" class="xref" title="Setting the In-Memory Log Buffer Size">Setting the In-Memory Log Buffer Size</a>.

- On-disk location of your log files. See <a href="enabletxn.md#splittingdata" class="xref" title="Identifying Specific File Locations">Identifying Specific File Locations</a>.

### Setting the Log File Size

Whenever a pre-defined amount of data is written to a log file (10 MB by default), DB stops using the current log file and starts writing to a new file. You can change the maximum amount of data contained in each log file by using the `EnvironmentConfig.setMaxLogFileSize()` method. Note that this method can be used at any time during an application's lifetime.

Setting the log file size to something larger than its default value is largely a matter of convenience and a reflection of the application's preference in backup media and frequency. However, if you set the log file size too low relative to your application's traffic patterns, you can cause yourself trouble.

From a performance perspective, setting the log file size to a low value can cause your active transactions to pause their writing activities more frequently than would occur with larger log file sizes. Whenever a transaction completes the log buffer is flushed to disk. Normally other transactions can continue to write to the log buffer while this flush is in progress. However, when one log file is being closed and another created, all transactions must cease writing to the log buffer until the switch over is completed.

Beyond performance concerns, using smaller log files can cause you to use more physical files on disk. As a result, your application could run out of log sequence numbers, depending on how busy your application is.

Every log file is identified with a 10 digit number. Moreover, the maximum number of log files that your application is allowed to create in its lifetime is 2,000,000,000.

For example, if your application performs 6,000 transactions per second for 24 hours a day, and you are logging 500 bytes of data per transaction into 10 MB log files, then you will run out of log files in around 221 years:

``` c
     (10 * 2^20 * 2000000000) / (6000 * 500 * 365 * 60 *60 * 24) = 221 
```

However, if you were writing 2000 bytes of data per transaction, and using 1 MB log files, then the same formula shows you running out of log files in 5 years time.

All of these time frames are quite long, to be sure, but if you do run out of log files after, say, 5 years of continuous operations, then you must reset your log sequence numbers. To do so:

1.  Backup your databases as if to prepare for catastrophic failure. See <a href="backuprestore.md" class="xref" title="Backup Procedures">Backup Procedures</a> for more information.

2.  Reset the log file's sequence number using the <span class="command">**db_load**</span> utility's `-r` option.

3.  Remove all of the log files from your environment. Note that this is the only situation in which all of the log files are removed from an environment; in all other cases, at least a single log file is retained.

4.  Restart your application.

### Configuring the Logging Region Size

The logging subsystem's default region size is 60 KB. The logging region is used to store filenames, and so you may need to increase its size if a large number of files (that is, if you have a very large number of databases) will be opened and registered with DB's log manager.

You can set the size of your logging region by using the `EnvironmentConfig.setLogRegionSize()` method. Note that this method can only be called before the first environment handle for your application is opened.

### Configuring In-Memory Logging

It is possible to configure your logging subsystem such that logs are maintained entirely in memory. When you do this, you give up your transactional durability guarantee. Without log files, you have no way to run recovery so any system or software failures that you might experience can corrupt your databases.

However, by giving up your durability guarantees, you can greatly improve your application's throughput by avoiding the disk I/O necessary to write logging information to disk. In this case, you still retain your transactional atomicity, consistency, and isolation guarantees.

To configure your logging subsystem to maintain your logs entirely in-memory:

- Make sure your log buffer is capable of holding all log information that can accumulate during the longest running transaction. See <a href="logconfig.md#logbuffer" class="xref" title="Setting the In-Memory Log Buffer Size">Setting the In-Memory Log Buffer Size</a> for details.

- Do not run normal recovery when you open your environment. In this configuration, there are no log files available against which you can run recovery. As a result, if you specify recovery when you open your environment, it is ignored.

- Specify `true` to the `EnvironmentConfig.setLogInMemory()` method. Note that you must specify this before your application opens its first environment handle.

For example:

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

    // Specify in-memory logging
    myEnvConfig.setLogInMemory(true);   

    // Specify the in-memory log buffer size.
    myEnvConfig.setLogBufferSize(10 * 1024 * 1024); 

    myEnv = new Environment(new File("/my/env/home"),
                              myEnvConfig);

    // From here, you open databases, create transactions and 
    // perform database operations exactly as you would if you 
    // were logging to disk. This part is omitted for brevity.  
```

### Setting the In-Memory Log Buffer Size

When your application is configured for on-disk logging (the default behavior for transactional applications), log information is stored in-memory until the storage space fills up, or a transaction commit forces the log information to be flushed to disk.

It is possible to increase the amount of memory available to your file log buffer. Doing so improves throughput for long-running transactions, or for transactions that produce a large amount of data.

When you have your logging subsystem configured to maintain your log entirely in memory (see <a href="logconfig.md#inmemorylogging" class="xref" title="Configuring In-Memory Logging">Configuring In-Memory Logging</a>), it is very important to configure your log buffer size because the log buffer must be capable of holding all log information that can accumulate during the longest running transaction. You must make sure that the in-memory log buffer size is large enough that no transaction will ever span the entire buffer. You must also avoid a state where the in-memory buffer is full and no space can be freed because a transaction that started the first log "file" is still active.

When your logging subsystem is configured for on-disk logging, the default log buffer space is 32 KB. When in-memory logging is configured, the default log buffer space is 1 MB.

You can increase your log buffer space using the `EnvironmentConfig.setLogBufferSize()` method. Note that this method can only be called before the first environment handle for your application is opened.
