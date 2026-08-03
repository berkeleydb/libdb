---
title: "In-Memory Transaction Example"
api-name: "In-Memory Transaction Example"
source: docs/gsg_txn/CXX/inmem_txnexample_c.html
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

For illustration purposes, we also modify this example so that uncommitted reads are no longer used to enable the `countRecords()` function. Instead, we simply provide a transaction handle to `countRecords()` so as to avoid the self-deadlock. Be aware that using a transaction handle here rather than uncommitted reads will work just as well as if we had continued to use uncommitted reads. However, the usage of the transaction handle here will probably cause more deadlocks than using read-uncommitted does, because more locking is being performed in this case.

To begin, we simplify the beginning of our example a bit. Because we no longer need an environment home directory, we can remove all the code that we used to determine path delimiters and include the `getopt` function. We can also remove our `usage()` function because we no longer require any command line arguments.

``` c
// File TxnGuideInMemory.cpp

// We assume an ANSI-compatible compiler
#include <db_cxx.h>
#include <pthread.h>
#include <iostream>

// Run 5 writers threads at a time.
#define NUMWRITERS 5

// Printing of pthread_t is implementation-specific, so we
// create our own thread IDs for reporting purposes.
int global_thread_num;
pthread_mutex_t thread_num_lock;

// Forward declarations
int countRecords(Db *, DbTxn *);
int openDb(Db **, const char *, const char *, DbEnv *, u_int32_t);
int usage(void);
void *writerThread(void *);  
```

Next, in our `main()`, we also eliminate some variables that this example no longer needs. In particular, we are able to remove the `dbHomeDir` and `fileName` variables. We also remove all our `getopt` code.

``` c
int
main(void)
{
    // Initialize our handles
    Db *dbp = NULL;
    DbEnv *envp = NULL;

    pthread_t writerThreads[NUMWRITERS];
    int i;
    u_int32_t envFlags;

    // Application name
    const char *progName = "TxnGuideInMemory";  
```

Next we create our environment as always. However, we add `DB_PRIVATE` to our environment open flags. This flag causes our environment to back regions using our application's heap memory rather than by using the filesystem. This is the first important step to keeping our DB data entirely in-memory.

We also remove the `DB_RECOVER` flag from the environment open flags. Because our databases, logs, and regions are maintained in-memory, there will never be anything to recover.

Note that we show the additional code here in **`bold.`**

``` c
    // Env open flags
    envFlags =
      DB_CREATE     |  // Create the environment if it does not exist
      DB_INIT_LOCK  |  // Initialize the locking subsystem
      DB_INIT_LOG   |  // Initialize the logging subsystem
      DB_INIT_TXN   |  // Initialize the transactional subsystem. This
                       // also turns on logging.
      DB_INIT_MPOOL |  // Initialize the memory pool (in-memory cache)
      DB_PRIVATE    |  // Region files are not backed by the filesystem.
                       // Instead, they are backed by heap memory.
      DB_THREAD;       // Cause the environment to be free-threaded

    try {
        // Create the environment 
        envp = new DbEnv(0); 
```

Now we configure our environment to keep the log files in memory, increase the log buffer size to 10 MB, and increase our in-memory cache to 10 MB. These values should be more than enough for our application's workload.

``` c
        
                  // Specify in-memory logging
        envp->log_set_config(DB_LOG_IN_MEMORY, 1);

        // Specify the size of the in-memory log buffer.
        envp->set_lg_bsize(10 * 1024 * 1024);

        // Specify the size of the in-memory cache
        envp->set_cachesize(0, 10 * 1024 * 1024, 1); 
        
      
```

Next, we open the environment and setup our lock detection. This is identical to how the example previously worked, except that we do not provide a location for the environment's home directory.

``` c
        // Indicate that we want db to internally perform deadlock 
        // detection.  Also indicate that the transaction with 
        // the fewest number of write locks will receive the 
        // deadlock notification in the event of a deadlock.
        envp->set_lk_detect(DB_LOCK_MINWRITE);

        // Open the environment
        envp->open(NULL, envFlags, 0); 
```

When we call `openDb()`, which is what we use to open our database, we no not provide a database filename for the third parameter. When the filename is `NULL`, the database is not backed by the filesystem.

``` c
        // If we had utility threads (for running checkpoints or 
        // deadlock detection, for example) we would spawn those
        // here. However, for a simple example such as this,
        // that is not required.

        // Open the database
        openDb(&dbp, progName, NULL,
            envp, DB_DUPSORT);
        
```

After that, our `main()` function is unchanged, except that when we check for exceptions on the database open, we change the error message string so as to not reference the database filename.

``` c
        // Initialize a pthread mutex. Used to help provide thread ids.
        (void)pthread_mutex_init(&thread_num_lock, NULL);

        // Start the writer threads.
        for (i = 0; i < NUMWRITERS; i++)
            (void)pthread_create(
                &writerThreads[i], NULL,
                writerThread,
                (void *)dbp);

        // Join the writers
        for (i = 0; i < NUMWRITERS; i++)
            (void)pthread_join(writerThreads[i], NULL);

    } catch(DbException &e) {
        std::cerr << "Error opening database environment: "
                  << std::endl;
        std::cerr << e.what() << std::endl;
        return (EXIT_FAILURE);
    }

    try {
        // Close our database handle if it was opened.
        if (dbp != NULL)
            dbp->close(0);

        // Close our environment if it was opened.
        if (envp != NULL)
            envp->close(0);
    } catch(DbException &e) {
        std::cerr << "Error closing database and environment."
                  << std::endl;
        std::cerr << e.what() << std::endl;
        return (EXIT_FAILURE);
    }

    // Final status message and return.

    std::cout << "I'm all done." << std::endl;
    return (EXIT_SUCCESS);
} 
```

That completes `main()`. The bulk of our `writerThread()` function implementation is unchanged from the initial transaction example, except that we now pass `countRecords` a transaction handle, rather than configuring our application to perform uncommitted reads. Both mechanisms work well-enough for preventing a self-deadlock. However, the individual count in this example will tend to be lower than the counts seen in the previous transaction example, because `countRecords()` can no longer see records created but not yet committed by other threads.

``` c
// A function that performs a series of writes to a
// Berkeley DB database. The information written
// to the database is largely nonsensical, but the
// mechanism of transactional commit/abort and
// deadlock detection is illustrated here.
void *
writerThread(void *args)
{
    Db *dbp = (Db *)args;
    DbEnv *envp = dbp->get_env(dbp);

    int j, thread_num;
    int max_retries = 20;   // Max retry on a deadlock
    char *key_strings[] = {"key 1", "key 2", "key 3", "key 4",
                           "key 5", "key 6", "key 7", "key 8",
                           "key 9", "key 10"};

    // Get the thread number
    (void)pthread_mutex_lock(&thread_num_lock);
    global_thread_num++;
    thread_num = global_thread_num;
    (void)pthread_mutex_unlock(&thread_num_lock);

    // Initialize the random number generator 
    srand((u_int)pthread_self());

    // Perform 50 transactions
    for (int i=0; i<50; i++) {
        DbTxn *txn;
        bool retry = true;
        int retry_count = 0;
        // while loop is used for deadlock retries
        while (retry) {
            // try block used for deadlock detection and
            // general db exception handling
            try {

                // Begin our transaction. We group multiple writes in
                // this thread under a single transaction so as to
                // (1) show that you can atomically perform multiple 
                // writes at a time, and (2) to increase the chances 
                // of a deadlock occurring so that we can observe our 
                // deadlock detection at work.

                // Normally we would want to avoid the potential for 
                // deadlocks, so for this workload the correct thing 
                // would be to perform our puts with auto commit. But 
                // that would excessively simplify our example, so we 
                // do the "wrong" thing here instead.
                txn = NULL;
                envp->txn_begin(NULL, &txn, 0);
                // Perform the database write for this transaction.
                for (j = 0; j < 10; j++) {
                    Dbt key, value;
                    key.set_data(key_strings[j]);
                    key.set_size((strlen(key_strings[j]) + 1) *
                        sizeof(char));

                    int payload = rand() + i;
                    value.set_data(&payload);
                    value.set_size(sizeof(int));

                    // Perform the database put
                    dbp->put(txn, &key, &value, 0);
                }

                // countRecords runs a cursor over the entire database.
                // We do this to illustrate issues of deadlocking
                std::cout << thread_num <<  " : Found "
                          <<  countRecords(dbp, txn)
                          << " records in the database." << std::endl;

                std::cout << thread_num <<  " : committing txn : " << i
                          << std::endl;

                // commit
                try {
                    txn->commit(0);
                    retry = false;
                    txn = NULL;
                } catch (DbException &e) {
                    std::cout << "Error on txn commit: "
                              << e.what() << std::endl;
                }
            } catch (DbDeadlockException &de) {
                // First thing we MUST do is abort the transaction.
                if (txn != NULL)
                    (void)txn->abort();

                // Now we decide if we want to retry the operation.
                // If we have retried less than max_retries,
                // increment the retry count and goto retry.
                if (retry_count < max_retries) {
                    std::cout << "############### Writer " << thread_num
                              << ": Got DB_LOCK_DEADLOCK.\n"
                              << "Retrying write operation."
                              << std::endl;
                    retry_count++;
                    retry = true;
                 } else {
                    // Otherwise, just give up.
                    std::cerr << "Writer " << thread_num
                              << ": Got DeadLockException and out of "
                              << "retries. Giving up." << std::endl;
                    retry = false;
                 }
           } catch (DbException &e) {
                std::cerr << "db put failed" << std::endl;
                std::cerr << e.what() << std::endl;
                if (txn != NULL)
                    txn->abort();
                retry = false;
           } catch (std::exception &ee) {
            std::cerr << "Unknown exception: " << ee.what() << std::endl;
            return (0);
          }
        }
    }
    return (0);
} 
```

Next we update `countRecords()`. The only difference here is that we no longer specify `DB_READ_UNCOMMITTED` when we open our cursor. Note that even this minor change is not required. If we do not configure our database to support uncommitted reads, `DB_READ_UNCOMMITTED` on the cursor open will be silently ignored. However, we remove the flag anyway from the cursor open so as to avoid confusion.

``` c
int
countRecords(Db *dbp, DbTxn *txn)
{

    Dbc *cursorp = NULL;
    int count = 0;

    try {
        // Get the cursor
        dbp->cursor(txn, &cursorp, 0);

        Dbt key, value;
        while (cursorp->get(&key, &value, DB_NEXT) == 0) {
            count++;
        }
    } catch (DbDeadlockException &de) {
        std::cerr << "countRecords: got deadlock" << std::endl;
        cursorp->close();
        throw de;
    } catch (DbException &e) {
        std::cerr << "countRecords error:" << std::endl;
        std::cerr << e.what() << std::endl;
    }

    if (cursorp != NULL) {
        try {
            cursorp->close();
        } catch (DbException &e) {
            std::cerr << "countRecords: cursor close failed:" << std::endl;
            std::cerr << e.what() << std::endl;
        }
    }

    return (count);
} 
```

Finally, we update `openDb()`. This involves removing `DB_READ_UNCOMMITTED` from the open flags.

``` c
// Open a Berkeley DB database
int
openDb(Db **dbpp, const char *progname, const char *fileName,
  DbEnv *envp, u_int32_t extraFlags)
{
    int ret;
    u_int32_t openFlags;

    try {
        Db *dbp = new Db(envp, 0);

        // Point to the new'd Db
        *dbpp = dbp;

        if (extraFlags != 0)
            ret = dbp->set_flags(extraFlags);

        // Now open the database
        openFlags = DB_CREATE   |      // Allow database creation
                    DB_THREAD        |        
                    DB_AUTO_COMMIT;    // Allow auto commit

        dbp->open(NULL,       // Txn pointer
                  fileName,   // File name
                  NULL,       // Logical db name
                  DB_BTREE,   // Database type (using btree)
                  openFlags,  // Open flags
                  0);         // File mode. Using defaults
    } catch (DbException &e) {
        std::cerr << progname << ": openDb: db open failed:" << std::endl;
        std::cerr << e.what() << std::endl;
        return (EXIT_FAILURE);
    }

    return (EXIT_SUCCESS);
} 
```

This completes our in-memory transactional example. If you would like to experiment with this code, you can find the example in the following location in your DB distribution:

``` c
DB_INSTALL/examples_cxx/txn_guide
```
