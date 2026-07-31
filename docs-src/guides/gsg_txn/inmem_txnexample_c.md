---
title: "In-Memory Transaction Example"
api-name: "In-Memory Transaction Example"
source: docs/gsg_txn/C/inmem_txnexample_c.html
---
## In-Memory Transaction Example

DB is sometimes used for applications that simply need to cache data retrieved from some other location (such as a remote database server). DB is also often used in embedded systems.

In both cases, applications may want to use transactions for atomicity, consistency, and isolation guarantees, but they may also want to forgo the durability guarantee entirely. In doing so, they can keep their DB environment and databases entirely in-memory so as to avoid the performance impact of unneeded disk I/O.

To do this:

- Refrain from specifying a home directory when you open your environment. The exception to this is if you are using the `DB_CONFIG` configuration file — in that case you must identify the environment's home directory so that the configuration file can be found.

- Configure your environment to back your regions from system memory instead of the filesystem.

- Configure your logging subsystem such that log files are kept entirely in-memory.

- Increase the size of your in-memory log buffer so that it is large enough to hold the largest set of concurrent write operations.

- Increase the size of your in-memory cache so that it can hold your entire data set. You do not want your cache to page to disk.

- Do not specify a file name when you open your database(s).

As an example, this section takes the transaction example provided in <a href="txnexample_c.md" class="xref" title="Transaction Example">Transaction Example</a> and it updates that example so that the environment, database, log files, and regions are all kept entirely in-memory.

For illustration purposes, we also modify this example so that uncommitted reads are no longer used to enable the `count_records()` function. Instead, we simply provide a transaction handle to `count_records()` so as to avoid the self-deadlock. Be aware that using a transaction handle here rather than uncommitted reads will work just as well as if we had continued to use uncommitted reads. However, the usage of the transaction handle here will probably cause more deadlocks than using read-uncommitted does, because more locking is being performed in this case.

To begin, we simplify the beginning of our example a bit. Because we no longer need an environment home directory, we can remove all the code that we used to determine path delimiters and include the `getopt` function. We can also remove our `usage()` function because we no longer require any command line arguments.

``` c
/* File: txn_guide_inmemory.c */

/* We assume an ANSI-compatible compiler */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <db.h>

/* Run 5 writers threads at a time. */
#define NUMWRITERS 5

/*
 * Printing of pthread_t is implementation-specific, so we
 * create our own thread IDs for reporting purposes.
 */
int global_thread_num;
pthread_mutex_t thread_num_lock;

/* Forward declarations */
int count_records(DB *, DB_TXN *);
int open_db(DB **, const char *, const char *, DB_ENV *, u_int32_t);
int writer_thread(void *);  
```

Next, in our `main()`, we also eliminate some variables that this example no longer needs. In particular, we are able to remove the `db_home_dir` and `file_name` variables. We also remove all our `getopt` code.

``` c
int
main(void)
{
    /* Initialize our handles */
    DB *dbp = NULL;
    DB_ENV *envp = NULL;

    pthread_t writer_threads[NUMWRITERS];
    int i, ret, ret_t;
    u_int32_t env_flags;

    /* Application name */
    const char *prog_name = "txn_guide_inmemory";  
```

Next we create our environment as always. However, we add `DB_PRIVATE` to our environment open flags. This flag causes our environment to back regions using our application's heap memory rather than by using the filesystem. This is the first important step to keeping our DB data entirely in-memory.

We also remove the `DB_RECOVER` flag from the environment open flags. Because our databases, logs, and regions are maintained in-memory, there will never be anything to recover.

Note that we show the additional code here in **`bold.`**

``` c
    /* Create the environment */
    ret = db_env_create(&envp, 0);
    if (ret != 0) {
        fprintf(stderr, "Error creating environment handle: %s\n",
            db_strerror(ret));
        goto err;
    }

    env_flags =
      DB_CREATE     |  /* Create the environment if it does not exist */ 
      DB_INIT_LOCK  |  /* Initialize the locking subsystem */
      DB_INIT_LOG   |  /* Initialize the logging subsystem */
      DB_INIT_TXN   |  /* Initialize the transactional subsystem. This
                        * also turns on logging. */
      DB_INIT_MPOOL |  /* Initialize the memory pool (in-memory cache) */
      DB_PRIVATE    |  /* Region files are not backed by the filesystem. 
                        * Instead, they are backed by heap memory.  */
      DB_THREAD;       /* Cause the environment to be free-threaded */ 
```

Now we configure our environment to keep the log files in memory, increase the log buffer size to 10 MB, and increase our in-memory cache to 10 MB. These values should be more than enough for our application's workload.

``` c
        
              /* Specify in-memory logging */
    ret = envp->log_set_config(envp, DB_LOG_IN_MEMORY, 1);
    if (ret != 0) {
        fprintf(stderr, "Error setting log subsystem to in-memory: %s\n",
            db_strerror(ret));
        goto err;
    }

    /* 
     * Specify the size of the in-memory log buffer. 
     */
    ret = envp->set_lg_bsize(envp, 10 * 1024 * 1024);
    if (ret != 0) {
        fprintf(stderr, "Error increasing the log buffer size: %s\n",
            db_strerror(ret));
        goto err;
    }

    /* 
     * Specify the size of the in-memory cache. 
     */
    ret = envp->set_cachesize(envp, 0, 
        10 * 1024 * 1024, 1);
    if (ret != 0) {
        fprintf(stderr, "Error increasing the cache size: %s\n",
            db_strerror(ret));
        goto err;
    }
        
      
```

Next, we open the environment and setup our lock detection. This is identical to how the example previously worked, except that we do not provide a location for the environment's home directory.

``` c
    /*
     * Indicate that we want db to perform lock detection internally.
     * Also indicate that the transaction with the fewest number of
     * write locks will receive the deadlock notification in 
     * the event of a deadlock.
     */  
    ret = envp->set_lk_detect(envp, DB_LOCK_MINWRITE);
    if (ret != 0) {
        fprintf(stderr, "Error setting lock detect: %s\n",
            db_strerror(ret));
        goto err;
    } 

    /* Now actually open the environment */
    ret = envp->open(envp, NULL, env_flags, 0);
    if (ret != 0) {
        fprintf(stderr, "Error opening environment: %s\n",
            db_strerror(ret));
        goto err;
    } 
```

When we call `open_db()`, which is what we use to open our database, we no not provide a database filename for the third parameter. When the filename is `NULL`, the database is not backed by the filesystem.

``` c
    /*
     * If we had utility threads (for running checkpoints or 
     * deadlock detection, for example) we would spawn those
     * here. However, for a simple example such as this,
     * that is not required.
     */

    /* Open the database */
    ret = open_db(&dbp, prog_name, NULL, 
      envp, DB_DUPSORT);
    if (ret != 0)
        goto err; 
```

After that, our `main()` function is unchanged, except that when we close the database, we change the error message string so as to not reference the database filename.

``` c
    /* Initialize a pthread mutex. Used to help provide thread ids. */
    (void)pthread_mutex_init(&thread_num_lock, NULL);

    /* Start the writer threads. */
    for (i = 0; i < NUMWRITERS; i++)
        (void)pthread_create(
      &writer_threads[i], NULL, (void *)writer_thread, (void *)dbp);

    /* Join the writers */
    for (i = 0; i < NUMWRITERS; i++)
        (void)pthread_join(writer_threads[i], NULL);

err:
    /* Close our database handle, if it was opened. */
    if (dbp != NULL) {
        ret_t = dbp->close(dbp, 0);
        if (ret_t != 0) {
            fprintf(stderr, "%s database close failed.\n",
                db_strerror(ret_t));
            ret = ret_t;
        }
    }

    /* Close our environment, if it was opened. */
    if (envp != NULL) {
        ret_t = envp->close(envp, 0);
        if (ret_t != 0) {
            fprintf(stderr, "environment close failed: %s\n",
                db_strerror(ret_t));
                ret = ret_t;
        }
    }

    /* Final status message and return. */
    printf("I'm all done.\n");
    return (ret == 0 ? EXIT_SUCCESS : EXIT_FAILURE);
} 
```

That completes `main()`. The bulk of our `writer_thread()` function implementation is unchanged from the initial transaction example, except that we no longer check for `DB_KEYEXISTS` in our `DB->put()` return code. Because we are configuring for a completely in-memory database, there is no possibility that we can run this code against an existing database. Therefore, there is no way that `DB_KEYEXISTS` will be returned by `DB->put()`.

``` c
/* 
 * A function that performs a series of writes to a
 * Berkeley DB database. The information written
 * to the database is largely nonsensical, but the
 * mechanism of transactional commit/abort and
 * deadlock detection is illustrated here.
 */
int
writer_thread(void *args)
{
    DBT key, value;
    DB_TXN *txn;
    int i, j, payload, ret, thread_num;
    int retry_count, max_retries = 20;   /* Max retry on a deadlock */
    char *key_strings[] = {"key 1", "key 2", "key 3", "key 4",
                           "key 5", "key 6", "key 7", "key 8",
                           "key 9", "key 10"};

    DB *dbp = (DB *)args;
    DbEnv *envp = dbp-&gt;get_env();

    /* Get the thread number */
    (void)pthread_mutex_lock(&thread_num_lock);
    global_thread_num++;
    thread_num = global_thread_num;
    (void)pthread_mutex_unlock(&thread_num_lock);

    /* Initialize the random number generator */
    srand((u_int)pthread_self());

    /* Write 50 times and then quit */
    for (i = 0; i < 50; i++) {
        retry_count = 0; /* Used for deadlock retries */

retry:
        ret = envp->txn_begin(envp, NULL, &txn, 0);
        if (ret != 0) {
            envp->err(envp, ret, "txn_begin failed");
            return (EXIT_FAILURE);
        }
        for (j = 0; j < 10; j++) {
            /* Set up our key and values DBTs */
            memset(&key, 0, sizeof(DBT));
            key.data = key_strings[j];
            key.size = (strlen(key_strings[j]) + 1) * sizeof(char);

            memset(&value, 0, sizeof(DBT));
            payload = rand() + i;
            value.data = &payload;
            value.size = sizeof(int);

            /* Perform the database put. */
            switch (ret = dbp->put(dbp, txn, &key, &value, 0)) {
                case 0:
                    break;

                /*
                 * Here's where we perform deadlock detection. If 
                 * DB_LOCK_DEADLOCK is returned by the put operation, 
                 * then this thread has been chosen to break a deadlock.
                 * It must abort its operation, and optionally retry the
                 * put.
                 */
                case DB_LOCK_DEADLOCK:
                    /* 
                     * First that we MUST do is abort the 
                     * transaction.
                     */
                    (void)txn->abort(txn);
                    /*
                     * Now we decide if we want to retry the operation.
                     * If we have retried less than max_retries,
                     * increment the retry count and goto retry.
                     */
                    if (retry_count < max_retries) {
                        printf("Writer %i: Got DB_LOCK_DEADLOCK.\n", 
                            thread_num);
                        printf("Writer %i: Retrying write operation.\n",
                            thread_num);
                        retry_count++;
                        goto retry;
                    }
                    /*
                     * Otherwise, just give up.
                     */
                    printf("Writer %i: ", thread_num);
                    printf("Got DB_LOCK_DEADLOCK and out of retries.\n");
                    printf("Writer %i: Giving up.\n", thread_num);
                    return (EXIT_FAILURE);
                /* 
                 * If a generic error occurs, we simply abort the 
                 * transaction and exit the thread completely.
                 */
                default:
                    envp->err(envp, ret, "db put failed");
                    ret = txn->abort(txn);
                    if (ret != 0)
                        envp->err(envp, ret, "txn abort failed");
                    return (EXIT_FAILURE);
             } /** End case statement **/

        }   /** End for loop **/  
```

The only other change to `writer_thread()` is that we pass `count_records()` a transaction handle, rather than configuring our entire application for uncommitted reads. Both mechanisms work well-enough for preventing a self-deadlock. However, the individual count in this example will tend to be lower than the counts seen in the previous transaction example, because `count_records()` can no longer see records created but not yet committed by other threads.

``` c
        /* 
         * print the number of records found in the database. 
         * See count_records() for usage information.
         */
        printf("Thread %i. Record count: %i\n", thread_num, 
            count_records(dbp, txn));

        /* 
         * If all goes well, we can commit the transaction and
         * loop to the next transaction.
         */
        ret = txn->commit(txn, 0);
        if (ret != 0) {
            envp->err(envp, ret, "txn commit failed");
            return (EXIT_FAILURE);
        }
    }
    return (EXIT_SUCCESS);
} 
```

Next we update `count_records()`. The only difference here is that we no longer specify `DB_READ_UNCOMMITTED` when we open our cursor. Note that even this minor change is not required. If we do not configure our database to support uncommitted reads, `DB_READ_UNCOMMITTED` on the cursor open will be silently ignored. However, we remove the flag anyway from the cursor open so as to avoid confusion.

``` c
int
count_records(DB *dbp, DB_TXN *txn)
{
    DBT key, value;
    DBC *cursorp;
    int count, ret;

    cursorp = NULL;
    count = 0;

    /* Get the cursor */
    ret = dbp->cursor(dbp, txn, &cursorp, 0);
    if (ret != 0) {
        dbp->err(dbp, ret, "count_records: cursor open failed.");
        goto cursor_err;
    }

    /* Get the key DBT used for the database read */
    memset(&key, 0, sizeof(DBT));
    memset(&value, 0, sizeof(DBT));
    do {
        ret = cursorp->get(cursorp, &key, &value, DB_NEXT);
        switch (ret) {
            case 0:
                count++;
                break;
            case DB_NOTFOUND:
                break;
            default:
                dbp->err(dbp, ret, 
                    "Count records unspecified error");
                goto cursor_err;
        }
    } while (ret == 0);

cursor_err:
    if (cursorp != NULL) {
        ret = cursorp->close(cursorp);
        if (ret != 0) {
            dbp->err(dbp, ret,
                "count_records: cursor close failed.");
        }
    }

    return (count);
}
```

Finally, we update `open_db()`. This involves removing `DB_READ_UNCOMMITTED` from the open flags. We are also careful to change our database open error message to no longer use the `file_name` variable because that value will always be `NULL` for this example.

``` c
/* Open a Berkeley DB database */
int
open_db(DB **dbpp, const char *progname, const char *file_name,
  DB_ENV *envp, u_int32_t extra_flags)
{
    int ret;
    u_int32_t open_flags;
    DB *dbp;

    /* Initialize the DB handle */
    ret = db_create(&dbp, envp, 0);
    if (ret != 0) {
        fprintf(stderr, "%s: %s\n", progname,
                db_strerror(ret));
        return (EXIT_FAILURE);
    }

    /* Point to the memory malloc'd by db_create() */
    *dbpp = dbp;

    if (extra_flags != 0) {
        ret = dbp->set_flags(dbp, extra_flags);
        if (ret != 0) {
            dbp->err(dbp, ret, 
                "open_db: Attempt to set extra flags failed.");
            return (EXIT_FAILURE);
        }
    }

    /* Now open the database */
    open_flags = DB_CREATE        | /* Allow database creation */ 
                 DB_THREAD        |        
                 DB_AUTO_COMMIT;    /* Allow auto commit */

    ret = dbp->open(dbp,        /* Pointer to the database */
                    NULL,       /* Txn pointer */
                    file_name,  /* File name */
                    NULL,       /* Logical db name */
                    DB_BTREE,   /* Database type (using btree) */
                    open_flags, /* Open flags */
                    0);         /* File mode. Using defaults */
    if (ret != 0) {
        dbp->err(dbp, ret, "Database  open failed");
        return (EXIT_FAILURE);
    }
    return (EXIT_SUCCESS);
} 
```

This completes our in-memory transactional example. If you would like to experiment with this code, you can find the example in the following location in your DB distribution:

``` c
DB_INSTALL/examples_c/txn_guide
```
