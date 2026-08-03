---
title: "The Locking Subsystem"
api-name: "The Locking Subsystem"
source: docs/gsg_txn/C/lockingsubsystem.html
---
## The Locking Subsystem

<span class="sect2"> [Configuring the Locking Subsystem](lockingsubsystem.md#configuringlock) </span>

<span class="sect2"> [Configuring Deadlock Detection](lockingsubsystem.md#configdeadlkdetect) </span>

<span class="sect2"> [Resolving Deadlocks](lockingsubsystem.md#deadlockresolve) </span>

<span class="sect2"> [Setting Transaction Priorities](lockingsubsystem.md#setpriority) </span>

In order to allow concurrent operations, DB provides the locking subsystem. This subsystem provides inter- and intra- process concurrency mechanisms. It is extensively used by DB concurrent applications, but it can also be generally used for non-DB resources.

This section describes the locking subsystem as it is used to protect DB resources. In particular, issues on configuration are examined here. For information on using the locking subsystem to manage non-DB resources, see the *Berkeley DB Programmer's Reference Guide*.

### Configuring the Locking Subsystem

You initialize the locking subsystem by specifying `DB_INIT_LOCK` to the `DB_ENV->open()` method.

Before opening your environment, you can configure various values for your locking subsystem. Note that these limits can only be configured before the environment is opened. Also, these methods configure the entire environment, not just a specific environment handle.

Finally, each bullet below identifies the `DB_CONFIG` file parameter that can be used to specify the specific locking limit. If used, these `DB_CONFIG` file parameters override any value that you might specify using the environment handle.

The limits that you can configure are as follows:

- The number of lockers supported by the environment. This value is used by the environment when it is opened to estimate the amount of space that it should allocate for various internal data structures. By default, 1,000 lockers are supported.

  To configure this value, use the `DB_ENV->set_memory_init()` method to configure the `DB_MEM_LOCKER` structure.

  As an alternative to this method, you can configure this value using the `DB_CONFIG` file's `set_lk_max_lockers` parameter.

- The number of locks supported by the environment. By default, 1,000 locks are supported.

  To configure this value, use the `DB_ENV->set_memory_init()` method to configure the `DB_MEM_LOCK` structure.

  As an alternative to this method, you can configure this value using the `DB_CONFIG` file's `set_lk_max_locks` parameter.

- The number of locked objects supported by the environment. By default, 1,000 objects can be locked.

  To configure this value, use the `DB_ENV->set_memory_init()` method to configure the `DB_MEM_LOCKOBJECT` structure.

  As an alternative to this method, you can configure this value using the `DB_CONFIG` file's `set_lk_max_objects` parameter.

For a definition of lockers, locks, and locked objects, see <a href="blocking_deadlocks.md#lockresources" class="xref" title="Lock Resources">Lock Resources</a>.

For example, to configure the number of locks that your environment can use:

``` c
#include <stdio.h>
#include <stdlib.h>

#include "db.h"

int
main(void)
{
    int ret, ret_c;
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

    env_flags = DB_CREATE     |  /* If the environment does not
                                  * exist, create it. */
                DB_INIT_LOCK  |  /* Initialize locking */
                DB_INIT_LOG   |  /* Initialize logging */
                DB_INIT_MPOOL |  /* Initialize the cache */
                DB_THREAD     |  /* Free-thread the env handle. */
                DB_INIT_TXN;     /* Initialize transactions */

    /* Configure max locks */
    ret = envp->set_memory_init(envp, DB_MEM_LOCK, 5000);
    if (ret != 0) {
        fprintf(stderr, "Error configuring locks: %s\n",
            db_strerror(ret));
        goto err;
    }

    /* Open the environment. */
    ret = envp->open(envp, db_home_dir, env_flags, 0);
    if (ret != 0) {
        fprintf(stderr, "Error opening environment: %s\n",
            db_strerror(ret));
        goto err;
    }

err:
    /* Close the environment */
    if (envp != NULL) {
        ret_c = envp->close(envp, 0);
        if (ret_c != 0) {
            fprintf(stderr, "environment close failed: %s\n",
                db_strerror(ret_c));
            ret = ret_c;
        }
    }

    return (ret == 0 ? EXIT_SUCCESS : EXIT_FAILURE);
} 
```

### Configuring Deadlock Detection

In order for DB to know that a deadlock has occurred, some mechanism must be used to perform deadlock detection. There are three ways that deadlock detection can occur:

1.  Allow DB to internally detect deadlocks as they occur.

    To do this, you use `DB_ENV->set_lk_detect()`. This method causes DB to walk its internal lock table looking for a deadlock whenever a lock request is blocked. This method also identifies how DB decides which lock requests are rejected when deadlocks are detected. For example, DB can decide to reject the lock request for the transaction that has the most number of locks, the least number of locks, holds the oldest lock, holds the most number of write locks, and so forth (see the API reference documentation for a complete list of the lock detection policies).

    You can call this method at any time during your application's lifetime, but typically it is used before you open your environment.

    Note that how you want DB to decide which thread of control should break a deadlock is extremely dependent on the nature of your application. It is not unusual for some performance testing to be required in order to make this determination. That said, a transaction that is holding the most number of locks is usually indicative of the transaction that has performed the most amount of work. Frequently you will not want a transaction that has performed a lot of work to abandon its efforts and start all over again. It is not therefore uncommon for application developers to initially select the transaction with the <span class="emphasis">*minimum*</span> number of write locks to break the deadlock.

    Using this mechanism for deadlock detection means that your application will never have to wait on a lock before discovering that a deadlock has occurred. However, walking the lock table every time a lock request is blocked can be expensive from a performance perspective.

2.  Use a dedicated thread or external process to perform deadlock detection. Note that this thread must be performing no other database operations beyond deadlock detection.

    To externally perform lock detection, you can use either the `DB_ENV->lock_detect()` method, or use the <span class="command">**db_deadlock**</span> command line utility. This method (or command) causes DB to walk the lock table looking for deadlocks.

    Note that like `DB_ENV->set_lk_detect()`, you also use this method (or command line utility) to identify which lock requests are rejected in the event that a deadlock is detected.

    Applications that perform deadlock detection in this way typically run deadlock detection between every few seconds and a minute. This means that your application may have to wait to be notified of a deadlock, but you also save the overhead of walking the lock table every time a lock request is blocked.

3.  Lock timeouts.

    You can configure your locking subsystem such that it times out any lock that is not released within a specified amount of time. To do this, use the `DB_ENV->set_timeout()` method. Note that lock timeouts are only checked when a lock request is blocked or when deadlock detection is otherwise performed. Therefore, a lock can have timed out and still be held for some length of time until DB has a reason to examine its locking tables.

    Be aware that extremely long-lived transactions, or operations that hold locks for a long time, may be inappropriately timed out before the transaction or operation has a chance to complete. You should therefore use this mechanism only if you know your application will hold locks for very short periods of time.

For example, to configure your application such that DB checks the lock table for deadlocks every time a lock request is blocked:

``` c
#include <stdio.h>
#include <stdlib.h>

#include "db.h"

int
main(void)
{
    int ret, ret_c;
    u_int32_t db_flags, env_flags;
    DB *dbp;
    DB_ENV *envp;
    DB_TXN *txn;
    const char *db_home_dir = "/tmp/myEnvironment";
    const char *file_name = "mydb.db";
    
    envp = NULL;

    /* Open the environment */
    ret = db_env_create(&envp, 0);
    if (ret != 0) {
        fprintf(stderr, "Error creating environment handle: %s\n",
            db_strerror(ret));
        return (EXIT_FAILURE);
    }

    env_flags = DB_CREATE     |  /* If the environment does not
                                  * exist, create it. */
                DB_INIT_LOCK  |  /* Initialize locking */
                DB_INIT_LOG   |  /* Initialize logging */
                DB_INIT_MPOOL |  /* Initialize the cache */
                DB_THREAD     |  /* Free-thread the env handle. */
                DB_INIT_TXN;     /* Initialize transactions */

    /*
     * Configure db to perform deadlock detection internally, and to
     * choose the transaction that has performed the least amount of
     * writing to break the deadlock in the event that one is detected.
     */
    ret = envp->set_lk_detect(envp, DB_LOCK_MINWRITE);
    if (ret != 0) {
        fprintf(stderr, "Error setting lk detect: %s\n",
            db_strerror(ret));
        goto err;
    }

    ret = envp->open(envp, db_home_dir, env_flags, 0);
    if (ret != 0) {
        fprintf(stderr, "Error opening environment: %s\n",
            db_strerror(ret));
        goto err;
    }

    /* 
     * From here, you open your databases, proceed with your 
     * database operations, and respond to deadlocks as 
     * is normal (omitted for brevity).
     */

     ...  
```

Finally, the following command line call causes deadlock detection to be run against the environment contained in `/export/dbenv`. The transaction with the youngest lock is chosen to break the deadlock:

``` c
> /usr/local/db_install/bin/db_deadlock -h /export/dbenv -a y
```

For more information, see the <a href="http://download.oracle.com/docs/cd/E17076_02/html/api_reference/C/db_deadlock.html" class="ulink" target="_top"><code class="literal">db_deadlock</code> reference documentation.</a>

### Resolving Deadlocks

When DB determines that a deadlock has occurred, it will select a thread of control to resolve the deadlock and then return `DB_LOCK_DEADLOCK` to that thread. If a deadlock is detected, the thread must:

1.  Cease all read and write operations.

2.  Close all open cursors.

3.  Abort the transaction.

4.  Optionally retry the operation. If your application retries deadlocked operations, the new attempt must be made using a new transaction.

### Note

If a thread has deadlocked, it may not make any additional database calls using the handle that has deadlocked.

For example:

``` c
retry:
    ret = envp->txn_begin(envp, NULL, &txn, 0);
    if (ret != 0) {
            envp->err(envp, ret, "txn_begin failed");
            return (EXIT_FAILURE);
    }
    ...
    /* key and data are Dbts. Their usage is omitted for brevity. */
    ...
    switch (ret = dbp->put(dbp, txn, &key, &data, 0)) {
        case 0:
            break;
        /* Deadlock handling goes here */
        case DB_LOCK_DEADLOCK:
            /* Abort the transaction */
            (void)txn->abort(txn);

            /* 
             * retry_count is a counter used to identify how many times
             * we've retried this operation. To avoid the potential for
             * endless looping, we won't retry more than 
             * MAX_DEADLOCK_RETRIES times.
             */
            if (retry_count < MAX_DEADLOCK_RETRIES) {
                printf("Got DB_LOCK_DEADLOCK.\n");
                printf("Retrying write operation.\n");
                    retry_count++;
                    goto retry;
             }
             printf("Got DB_LOCK_DEADLOCK and out of retries.");
             printf("Giving up.\n");
             return (EXIT_FAILURE);
         default:
            /* If some random database error occurs, we just give up */
            envp->err(envp, ret, "db put failed");
            ret = txn->abort(txn);
            if (ret != 0) {
                envp->err(envp, ret, "txn abort failed");
                return (EXIT_FAILURE);
             } 
         }
    /* If all goes well, commit the transaction */
    ret = txn->commit(txn, 0);
    if (ret != 0) {
        envp->err(envp, ret, "txn commit failed");
        return (EXIT_FAILURE);
    }

    return (EXIT_SUCCESS); 
```

### Setting Transaction Priorities

Normally when a thread of control must be selected to resolve a deadlock, DB decides which thread will perform the resolution; you have no way of knowing in advance which thread will be selected to resolve the deadlock.

However, there may be situations where you know it is better for one thread to resolve a deadlock over another thread. As an example, if you have a background thread running data management activities, and another thread responding to user requests, you might want deadlock resolution to occur in the background thread because you can better afford the throughput costs there. Under these circumstances, you can identify which thread of control will be selected for resolved deadlocks by setting a transaction priorities.

When two transactions are deadlocked, DB will abort the transaction with the lowest priority. By default, every transaction is given a priority of 100. However, you can set a different priority on a transaction-by-transaction basis by using the `DB_TXN->set_priority()` method.

When two or more transactions are tied for the lowest priority, the tie is broken based on the policy provided to the `DB_ENV->lock_detect()` method's `atype` parameter.

A transaction's priority can be changed at any time after the transaction handle has been created and before the transaction has been resolved (committed or aborted). For example:

``` c
#include <stdio.h>
#include <stdlib.h>

#include "db.h"

int
main(void)
{
    int ret, ret_c;
    u_int32_t db_flags, env_flags;
    DB *dbp;
    DB_ENV *envp;
    DBT key, data;
    DB_TXN *txn;

    ...
    // Open the environment and database as normal.
    // Omitted for brevity
    ...

    /* Get the txn handle */
    txn = NULL;
    ret = envp->txn_begin(envp, NULL, &txn, 0);
    if (ret != 0) {
        envp->err(envp, ret, "Transaction begin failed.");
        goto err;
    }

    ret = txn->set_priority(txn, 200);
    if (ret != 0) {
        envp->err(envp, ret, "Transaction set_priority failed.");
        goto err;
    }

    /*
     * Perform the database write. If this fails, abort the transaction.
     */
    ret = dbp->put(dbp, txn, &key, &data, 0);
    if (ret != 0) {
        envp->err(envp, ret, "Database put failed.");
        txn->abort(txn);
        goto err;
    }

    /* 
     * Commit the transaction.
     */ 
    ret = txn->commit(txn, 0);
    if (ret != 0) {
        envp->err(envp, ret, "Transaction commit failed.");
        goto err;
    }

err:
    ...
    // Close the database and environment here, and exit the application.
    // Omitted for brevity.
} 
```
