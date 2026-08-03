---
title: "Recovery Procedures"
api-name: "Recovery Procedures"
source: docs/gsg_txn/C/recovery.html
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

- Provide the `DB_RECOVER` flag when you open your environment.

You can also run recovery by pausing or shutting down your application and using the <span class="command">**db_recover**</span> command line utility.

For example:

``` c
#include <stdio.h>
#include <stdlib.h>
#include "db.h"     

int
main(void)
{
    int ret;
    u_int32_t env_flags;
    DB_ENV *envp;
    const char *db_home_dir = "/tmp/myEnvironment";

    envp = NULL;

    /* Open the environment */
    ret = db_env_create(&envp, 0);
    if (ret != 0) {
        fprintf(stderr, "Error creating environment handle: %s\n",
            db_strerror(ret));
        return (EXIT_FAILURE);
    }

    /* Open the environment, specifying that recovery is to be run. */
    env_flags = DB_CREATE     |  /* If the environment does not
                                  * exist, create it. */
                DB_INIT_LOCK  |  /* Initialize locking */
                DB_INIT_LOG   |  /* Initialize logging */
                DB_INIT_MPOOL |  /* Initialize the cache */
                DB_THREAD     |  /* Free-thread the env handle. */
                DB_INIT_TXN   |  /* Initialize transactions */
                DB_RECOVER;      /* Run normal recovery */

    /* Open the environment. */
    ret = envp->open(envp, db_home_dir, env_flags, 0);
    if (ret != 0) {
        fprintf(stderr, "Error opening environment: %s\n",
            db_strerror(ret));
        goto err;
    }

    ...
    /*
     * All other operations are identical from here. Notice, however,
     * that we have not created any other threads of control before
     * recovery is complete. You want to run recovery for
     * the first thread in your application that opens an environment,
     * but not for any subsequent threads.
     */ 
```

### Catastrophic Recovery

Use catastrophic recovery when you are recovering your databases from a previously created backup. Note that to restore your databases from a previous backup, you should copy the backup to a new environment directory, and then run catastrophic recovery. Failure to do so can lead to the internal database structures being out of sync with your log files.

Catastrophic recovery must be run single-threaded.

To run catastrophic recovery:

- Shutdown all database operations.

- Restore the backup to an empty directory.

- Provide the `DB_RECOVER_FATAL` flag when you open your environment. This environment open must be single-threaded.

You can also run recovery by pausing or shutting down your application and using the <span class="command">**db_recover**</span> command line utility with the the `-c` option.

Note that catastrophic recovery examines every available log file — not just those log files created since the last checkpoint as is the case for normal recovery. For this reason, catastrophic recovery is likely to take longer than does normal recovery.

For example:

``` c
#include <stdio.h>
#include <stdlib.h>
#include "db.h"     

int
main(void)
{
    int ret;
    u_int32_t env_flags;
    DB_ENV *envp;
    const char *db_home_dir = "/tmp/myEnvironment";

    envp = NULL;

    /* Open the environment */
    ret = db_env_create(&envp, 0);
    if (ret != 0) {
        fprintf(stderr, "Error creating environment handle: %s\n",
            db_strerror(ret));
        return (EXIT_FAILURE);
    }

    /* Open the environment, specifying that recovery is to be run. */
    env_flags = DB_CREATE     |  /* If the environment does not
                                  * exist, create it. */
                DB_INIT_LOCK  |  /* Initialize locking */
                DB_INIT_LOG   |  /* Initialize logging */
                DB_INIT_MPOOL |  /* Initialize the cache */
                DB_THREAD     |  /* Free-thread the env handle. */
                DB_INIT_TXN   |  /* Initialize transactions */
                DB_RECOVER_FATAL;      /* Run catastrophic recovery */

    /* Open the environment. */
    ret = envp->open(envp, db_home_dir, env_flags, 0);
    if (ret != 0) {
        fprintf(stderr, "Error opening environment: %s\n",
            db_strerror(ret));
        goto err;
    }

    ...
    /*
     * All other operations are identical from here. Notice, however,
     * that we have not created any other threads of control before
     * recovery is complete. You want to run recovery for
     * the first thread in your application that opens an environment,
     * but not for any subsequent threads.
     */ 
```
