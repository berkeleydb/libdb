---
title: "Recovery Procedures"
api-name: "Recovery Procedures"
source: docs/gsg_txn/JAVA/recovery.html
---
## Recovery Procedures

<span class="sect2"> [Normal Recovery](recovery.md#normalrecovery) </span>

<span class="sect2"> [Catastrophic Recovery](recovery.md#catastrophicrecovery) </span>

DB supports two types of recovery:

- Normal recovery, which is run when your environment is opened upon application startup, examines only those log records needed to bring the databases to a consistent state since the last checkpoint. Normal recovery starts with any logs used by any transactions active at the time of the last checkpoint, and examines all logs from then to the current logs.

- Catastrophic recovery, which is performed in the same way that normal recovery is except that it examines all available log files. You use catastrophic recovery to restore your databases from a previously created backup.

Of these two, normal recovery should be considered a routine matter; in fact you should run normal recovery whenever you start up your application.

Catastrophic recovery is run whenever you have lost or corrupted your database files and you want to restore from a backup. You also run catastrophic recovery when you create a hot backup (see <a href="hotfailover.md" class="xref" title="Using Hot Failovers">Using Hot Failovers</a> for more information).

### Normal Recovery

Normal recovery examines the contents of your environment's log files, and uses this information to ensure that your database files are consistent relative to the information contained in the log files.

Normal recovery also recreates your environment's region files. This has the desired effect of clearing any unreleased locks that your application may have held at the time of an unclean application shutdown.

Normal recovery is run only against those log files created since the time of your last checkpoint. For this reason, your recovery time is dependent on how much data has been written since the last checkpoint, and therefore on how much log file information there is to examine. If you run checkpoints infrequently, then normal recovery can take a relatively long time.

### Note

You should run normal recovery every time you perform application startup.

To run normal recovery:

- Make sure all your environment handles are closed.

- Normal recovery <span class="emphasis">*must be*</span> single-threaded.

- Specify `true` to `EnvironmentConfig.setRunRecovery()` when you open your environment.

You can also run recovery by pausing or shutting down your application and using the <span class="command">**db_recover**</span> command line utility.

For example:

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

    // Run normal recovery
    myEnvConfig.setRunRecovery(true);

    myEnv = new Environment(new File("/my/env/home"),
                              myEnvConfig);

    // All other operations are identical from here. Notice, however,
    // that we have not created any other threads of control before
    // recovery is complete. You want to run recovery for
    // the first thread in your application that opens an environment,
    // but not for any subsequent threads. 

} catch (DatabaseException de) {
    // Exception handling goes here
} catch (FileNotFoundException fnfe) {
     // Exception handling goes here
}
```

### Catastrophic Recovery

Use catastrophic recovery when you are recovering your databases from a previously created backup. Note that to restore your databases from a previous backup, you should copy the backup to a new environment directory, and then run catastrophic recovery. Failure to do so can lead to the internal database structures being out of sync with your log files.

Catastrophic recovery must be run single-threaded.

To run catastrophic recovery:

- Shutdown all database operations.

- Restore the backup to an empty directory.

- Specify `true` to `EnvironmentConfig.setRunRecoveryFatal()` when you open your environment. This environment open must be single-threaded.

You can also run recovery by pausing or shutting down your application and using the <span class="command">**db_recover**</span> command line utility with the the `-c` option.

Note that catastrophic recovery examines every available log file — not just those log files created since the last checkpoint as is the case for normal recovery. For this reason, catastrophic recovery is likely to take longer than does normal recovery.

For example:

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

    // Run catastrophic recovery
    myEnvConfig.setRunFatalRecovery(true);

    myEnv = new Environment(new File("/my/env/home"),
                              myEnvConfig);

} catch (DatabaseException de) {
    // Exception handling goes here
} catch (FileNotFoundException fnfe) {
     // Exception handling goes here
}
```
