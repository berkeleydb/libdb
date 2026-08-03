---
title: "Chapter 5. Managing DB Files"
api-name: "Chapter 5. Managing DB Files"
source: docs/gsg_txn/JAVA/filemanagement.html
---
## Chapter 5. Managing DB Files

**Table of Contents**

<span class="sect1"> [Checkpoints](filemanagement.md#checkpoints) </span>

<span class="sect1"> [Backup Procedures](backuprestore.md) </span>

<span class="sect2"> [About Unix Copy Utilities](backuprestore.md#copyutilities) </span>

<span class="sect2"> [Offline Backups](backuprestore.md#standardbackup) </span>

<span class="sect2"> [Hot Backup](backuprestore.md#hotbackup) </span>

<span class="sect2"> [Incremental Backups](backuprestore.md#incrementalbackups) </span>

<span class="sect1"> [Recovery Procedures](recovery.md) </span>

<span class="sect2"> [Normal Recovery](recovery.md#normalrecovery) </span>

<span class="sect2"> [Catastrophic Recovery](recovery.md#catastrophicrecovery) </span>

<span class="sect1"> [Designing Your Application for Recovery](architectrecovery.md) </span>

<span class="sect2"> [Recovery for Multi-Threaded Applications](architectrecovery.md#multithreadrecovery) </span>

<span class="sect2"> [Recovery in Multi-Process Applications](architectrecovery.md#multiprocessrecovery) </span>

<span class="sect1"> [Using Hot Failovers](hotfailover.md) </span>

<span class="sect1"> [Removing Log Files](logfileremoval.md) </span>

<span class="sect1"> [Configuring the Logging Subsystem](logconfig.md) </span>

<span class="sect2"> [Setting the Log File Size](logconfig.md#logfilesize) </span>

<span class="sect2"> [Configuring the Logging Region Size](logconfig.md#logregionsize) </span>

<span class="sect2"> [Configuring In-Memory Logging](logconfig.md#inmemorylogging) </span>

<span class="sect2"> [Setting the In-Memory Log Buffer Size](logconfig.md#logbuffer) </span>

DB is capable of storing several types of files on disk:

- Data files, which contain the actual data in your database.

- Log files, which contain information required to recover your database in the event of a system or application failure.

- Region files, which contain information necessary for the overall operation of your application.

- Temporary files, which are created only under certain special circumstances. These files never need to be backed up or otherwise managed and so they are not a consideration for the topics described in this chapter. See <a href="enabletxn.md#security" class="xref" title="Security Considerations">Security Considerations</a> for more information on temporary files.

Of these, you must manage your data and log files by ensuring that they are backed up. You should also pay attention to the amount of disk space your log files are consuming, and periodically remove any unneeded files. Finally, you can optionally tune your logging subsystem to best suit your application's needs and requirements. These topics are discussed in this chapter.

## Checkpoints

Before we can discuss DB file management, we need to describe checkpoints. When databases are modified (that is, a transaction is committed), the modifications are recorded in DB's logs, but they are <span class="emphasis">*not*</span> necessarily reflected in the actual database files on disk.

This means that as time goes on, increasingly more data is contained in your log files that is not contained in your data files. As a result, you must keep more log files around than you might actually need. Also, any recovery run from your log files will take increasingly longer amounts of time, because there is more data in the log files that must be reflected back into the data files during the recovery process.

You can reduce these problems by periodically running a checkpoint against your environment. The checkpoint:

- Flushes dirty pages from the in-memory cache. This means that data modifications found in your in-memory cache are written to the database files on disk. Note that a checkpoint also causes data dirtied by an uncommitted transaction to also be written to your database files on disk. In this latter case, DB's normal recovery is used to remove any such modifications that were subsequently abandoned by your application using a transaction abort.

  Normal recovery is describe in <a href="recovery.md" class="xref" title="Recovery Procedures">Recovery Procedures</a>.

- Writes a checkpoint record.

- Flushes the log. This causes all log data that has not yet been written to disk to be written.

- Writes a list of open databases.

There are several ways to run a checkpoint. One way is to use the <span class="command">**db_checkpoint**</span> command line utility. (Note, however, that this command line utility cannot be used if your environment was opened using `EnvironmentConfig.setPrivate()`.)

You can also run a thread that periodically checkpoints your environment for you by calling the `Environment.checkpoint()` method.

Note that you can prevent a checkpoint from occurring unless more than a specified amount of log data has been written since the last checkpoint. You can also prevent the checkpoint from running unless more than a specified amount of time has occurred since the last checkpoint. These conditions are particularly interesting if you have multiple threads or processes running checkpoints.

For configuration information, see the <a href="http://download.oracle.com/docs/cd/E17076_02/html/java/com/sleepycat/db/CheckpointConfig.html" class="ulink" target="_top">CheckpointConfig Javadoc page.</a>

Note that running checkpoints can be quite expensive. DB must flush every dirty page to the backing database files. On the other hand, if you do not run checkpoints often enough, your recovery time can be unnecessarily long and you may be using more disk space than you really need. Also, you cannot remove log files until a checkpoint is run. Therefore, deciding how frequently to run a checkpoint is one of the most common tuning activity for DB applications.

For example, the following class performs a checkpoint every 60 seconds, so long as 500 kb of logging data has been written since the last checkpoint:

``` c
package db.txn;

import com.sleepycat.db.CheckpointConfig;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.Environment;

public class CheckPointer extends Thread
{
    private CheckpointConfig cpc = new CheckpointConfig();
    private Environment myEnv = null;
    private static boolean canRun = true;

    // Constructor.
    CheckPointer(Environment env) {
        myEnv = env;
        // Run a checkpoint only if 500 kbytes of log data has been 
        // written.
        cpc.setKBytes(500);
    }

    // Thread method that performs a checkpoint every
    // 60 seconds
    public void run () {
        while (canRun) {
            try {
                myEnv.checkpoint(cpc);
                sleep(60000);
            } catch (DatabaseException de) {
                System.err.println("Checkpoint error: " +
                    de.toString());
            } catch (InterruptedException e) {
                // Should never get here
                System.err.println("got interrupted exception");
            }
        }
    }

   public static void stopRunning() {
        canRun = false;
   }
} 
```

And you use this class as follows. Note that we add the call to shutdown the checkpoint thread in our application's shutdown code:

``` c
package db.txn;

import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;

import java.io.File;
import java.io.FileNotFoundException;

public class TryCheckPoint {

    private static String myEnvPath = "./";

    private static Environment myEnv = null;

    private static void usage() {
        System.out.println("TxnGuide [-h <env directory>]");
        System.exit(-1);
    }

    public static void main(String args[]) {
        try {
            // Parse the arguments list
            parseArgs(args);
            // Open the environment and databases
            openEnv();

            // Start the checkpoint thread
            CheckPointer cp = new CheckPointer(myEnv);
            cp.start();

            //////////////////////////////////
            // Do database work here as normal
            //////////////////////////////////

            // Once all database work is completed, stop the checkpoint
            // thread.
            CheckPointer.stopRunning();

            // Join the checkpoint thread in case it needs some time to 
            // cleanly shutdown.
            cp.join();

        } catch (Exception e) {
            System.err.println("TryCheckPoint: " + e.toString());
            e.printStackTrace();
        } finally {
            closeEnv();
        }
        System.out.println("All done.");
    }

    // Open an environment and databases
    private static void openEnv() throws DatabaseException {
        System.out.println("opening env");

        // Set up the environment.
        EnvironmentConfig myEnvConfig = new EnvironmentConfig();
        myEnvConfig.setAllowCreate(true);
        myEnvConfig.setInitializeCache(true);
        myEnvConfig.setInitializeLocking(true);
        myEnvConfig.setInitializeLogging(true);
        myEnvConfig.setTransactional(true);
        // EnvironmentConfig.setThreaded(true) is the default behavior 
        // in Java, so we do not have to do anything to cause the
        // environment handle to be free-threaded.

        try {
            // Open the environment
            myEnv = new Environment(new File(myEnvPath),    // Env home
                                    myEnvConfig);

            // Skipping the database opens and closes for brevity

        } catch (FileNotFoundException fnfe) {
            System.err.println("openEnv: " + fnfe.toString());
            System.exit(-1);
        }
    }

    // Close the environment and databases
    private static void closeEnv() {
        System.out.println("Closing env");
        if (myEnv != null ) {
            try {
                myEnv.close();
            } catch (DatabaseException e) {
                System.err.println("closeEnv: " + e.toString());
                e.printStackTrace();
            }
        }
    }

    private TryCheckPoint() {}

    private static void parseArgs(String args[]) {
        for(int i = 0; i < args.length; ++i) {
            if (args[i].startsWith("-")) {
                switch(args[i].charAt(1)) {
                    case 'h':
                        myEnvPath = new String(args[++i]);
                    break;
                    default:
                        usage();
                }
            }
        }
    }
} 
```
