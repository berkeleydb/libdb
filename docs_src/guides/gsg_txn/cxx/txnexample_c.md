---
title: "Transaction Example"
api-name: "Transaction Example"
source: docs/gsg_txn/CXX/txnexample_c.html
---
## Transaction Example

The following code provides a fully functional example of a multi-threaded transactional DB application. For improved portability across platforms, this examples uses pthreads to provide threading support.

The example opens an environment and database and then creates 5 threads, each of which writes 500 records to the database. The keys used for these writes are pre-determined strings, while the data is a random value. This means that the actual data is arbitrary and therefore uninteresting; we picked it only because it requires minimum code to implement and therefore will stay out of the way of the main points of this example.

Each thread writes 10 records under a single transaction before committing and writing another 10 (this is repeated 50 times). At the end of each transaction, but before committing, each thread calls a function that uses a cursor to read every record in the database. We do this in order to make some points about database reads in a transactional environment.

Of course, each writer thread performs deadlock detection as described in this manual. In addition, normal recovery is performed when the environment is opened.

We start with our normal `include` directives:

``` c
// File TxnGuide.cpp

// We assume an ANSI-compatible compiler
#include <db_cxx.h>
#include <pthread.h>
#include <iostream>

#ifdef _WIN32
extern int getopt(int, char * const *, const char *);
#else
#include <unistd.h>
#endif  
```

We also need a directive that we use to identify how many threads we want our program to create:

``` c
// Run 5 writers threads at a time.
#define NUMWRITERS 5 
```

Next we declare a couple of global variables (used by our threads), and we provide our forward declarations for the functions used by this example.

``` c
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

We now implement our usage function, which identifies our only command line parameter:

``` c
// Usage function
int
usage()
{
    std::cerr << " [-h <database_home_directory>]" << std::endl;
    return (EXIT_FAILURE);
}  
```

With that, we have finished up our program's housekeeping, and we can now move on to the main part of our program. As usual, we begin with `main()`. First we declare all our variables, and then we initialize our DB handles.

``` c
int
main(int argc, char *argv[])
{
    // Initialize our handles
    Db *dbp = NULL;
    DbEnv *envp = NULL; 

    pthread_t writerThreads[NUMWRITERS];
    int ch, i;
    u_int32_t envFlags;
    char *dbHomeDir;

    // Application name
    const char *progName = "TxnGuide";

    // Database file name
    const char *fileName = "mydb.db";  
```

Now we need to parse our command line. In this case, all we want is to know where our environment directory is. If the `-h` option is not provided when this example is run, the current working directory is used instead.

``` c
    // Parse the command line arguments
#ifdef _WIN32
    dbHomeDir = ".\\";
#else
    dbHomeDir = "./";
#endif
    while ((ch = getopt(argc, argv, "h:")) != EOF)
        switch (ch) {
        case 'h':
            dbHomeDir = optarg;
            break;
        case '?':
        default:
            return (usage());
        }  
```

Next we create our database handle, and we define our environment open flags. There are a few things to notice here:

- We specify `DB_RECOVER`, which means that normal recovery is run every time we start the application. This is highly desirable and recommended for most applications.

- We also specify `DB_THREAD`, which means our environment handle will be free-threaded. This is very important because we will be sharing the environment handle across threads.

``` c
    // Env open flags
    envFlags =
      DB_CREATE     |  // Create the environment if it does not exist
      DB_RECOVER    |  // Run normal recovery.
      DB_INIT_LOCK  |  // Initialize the locking subsystem
      DB_INIT_LOG   |  // Initialize the logging subsystem
      DB_INIT_TXN   |  // Initialize the transactional subsystem. This
                       // also turns on logging.
      DB_INIT_MPOOL |  // Initialize the memory pool (in-memory cache)
      DB_THREAD;       // Cause the environment to be free-threaded

    try {
        // Create and open the environment 
        envp = new DbEnv(0);  
```

Now we configure how we want deadlock detection performed. In our case, we will cause DB to perform deadlock detection by walking its internal lock tables looking for a block every time a lock is requested. Further, in the event of a deadlock, the thread that holds the youngest lock will receive the deadlock notification.

``` c
        // Indicate that we want db to internally perform deadlock 
        // detection.  Also indicate that the transaction with 
        // the fewest number of write locks will receive the 
        // deadlock notification in the event of a deadlock.
        envp->set_lk_detect(DB_LOCK_MINWRITE);  
```

Now we open our environment.

``` c
        // If we had utility threads (for running checkpoints or 
        // deadlock detection, for example) we would spawn those
        // here. However, for a simple example such as this,
        // that is not required.

        envp->open(dbHomeDir, envFlags, 0); 
```

Now we call the function that will open our database for us. This is not very interesting, except that you will notice that we are specifying `DB_DUPSORT`. This is required purely by the data that we are writing to the database, and it is only necessary if you run the application more than once without first deleting the environment.

The implementation of `open_db()` is described later in this section.

``` c
        // Open the database
        openDb(&dbp, progName, fileName, envp, DB_DUPSORT);  
```

Now we create our threads. In this example we are using pthreads for our threading package. A description of threading (beyond how it impacts DB usage) is beyond the scope of this manual. However, the things that we are doing here should be familiar to anyone who has prior experience with any threading package. We are simply initializing a mutex, creating our threads, and then joining our threads, which causes our program to wait until the joined threads have completed before continuing operations in the main thread.

``` c
        // Initialize a pthread mutex. Used to help provide thread ids.
        (void)pthread_mutex_init(&thread_num_lock, NULL);

        // Start the writer threads.
        for (i = 0; i < NUMWRITERS; i++)
            (void)pthread_create(&writerThreads[i], NULL,
                writerThread, (void *)dbp);

        // Join the writers
        for (i = 0; i < NUMWRITERS; i++)
            (void)pthread_join(writerThreads[i], NULL);

    } catch(DbException &e) {
        std::cerr << "Error opening database environment: "
                  << dbHomeDir << std::endl;
        std::cerr << e.what() << std::endl;
        return (EXIT_FAILURE);
    }  
```

Finally, to wrap up `main()`, we close out our database and environment handle, as is normal for any DB application. Notice that this is where our `err` label is placed in our application. If any database operation prior to this point in the program returns an error status, the program simply jumps to this point and closes our handles if necessary before exiting the application completely.

``` c
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

Now that we have completed `main()`, we need to implement the function that our writer threads will actually run. This is where the bulk of our transactional code resides.

We start as usual with variable declarations and initialization.

``` c
// A function that performs a series of writes to a
// Berkeley DB database. The information written
// to the database is largely nonsensical, but the
// mechanisms of transactional commit/abort and
// deadlock detection are illustrated here.
void *
writerThread(void *args)
{
    int j, thread_num;
    int max_retries = 20;   // Max retry on a deadlock
    char *key_strings[] = {"key 1", "key 2", "key 3", "key 4",
                           "key 5", "key 6", "key 7", "key 8",
                           "key 9", "key 10"};

    Db *dbp = (Db *)args;
    DbEnv *envp = dbp->get_env();  
```

Now we want a thread number for reporting purposes. It is possible to use the `pthread_t` value directly for this purpose, but how that is done unfortunately differs depending on the pthread implementation you are using. So instead we use a mutex-protected global variable to obtain a simple integer for our reporting purposes.

Note that we are also use this thread id for initializing a random number generator, which we do here. We use this random number generator for data generation.

``` c
    // Get the thread number
    (void)pthread_mutex_lock(&thread_num_lock);
    global_thread_num++;
    thread_num = global_thread_num;
    (void)pthread_mutex_unlock(&thread_num_lock); 
    
    // Initialize the random number generator 
    srand((u_int)pthread_self());  
```

Now we begin the loop that we use to write data to the database. Notice that in this top loop, we begin a new transaction. We will actually use 50 transactions per writer thread, although we will only ever have one active transaction per thread at a time. Within each transaction, we will perform 10 database writes.

By combining multiple writes together under a single transaction, we increase the likelihood that a deadlock will occur. Normally, you want to reduce the potential for a deadlock and in this case the way to do that is to perform a single write per transaction. To avoid deadlocks, we could be using auto commit to write to our database for this workload.

However, we want to show deadlock handling and by performing multiple writes per transaction we can actually observe deadlocks occurring. We also want to underscore the idea that you can combing multiple database operations together in a single atomic unit of work in order to improve the efficiency of your writes.

``` c
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
```

Now we begin the inner loop that we use to actually perform the write.

``` c
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
```

Having completed the inner database write loop, we could simply commit the transaction and continue on to the next block of 10 writes. However, we want to first illustrate a few points about transactional processing so instead we call our `countRecords()` function before calling the transaction commit. `countRecords()` uses a cursor to read every record in the database and return a count of the number of records that it found.

``` c
                // countRecords runs a cursor over the entire database.
                // We do this to illustrate issues of deadlocking
                std::cout << thread_num <<  " : Found " 
                          <<  countRecords(dbp, NULL)    
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
```

Finally, we finish our try block. Notice how we examine the exceptions to determine whether we need to abort (or abort/retry in the case of a deadlock) our current transaction.

``` c
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

We want to back up for a moment and take a look at the call to `countRecords()`. If you look at the `countRecords()` function prototype at the beginning of this example, you will see that the function's second parameter takes a transaction handle. However, our usage of the function here does not pass a transaction handle through to the function.

Because `countRecords()` reads every record in the database, if used incorrectly the thread will self-deadlock. The writer thread has just written 500 records to the database, but because the transaction used for that write has not yet been committed, each of those 500 records are still locked by the thread's transaction. If we then simply run a non-transactional cursor over the database from within the same thread that has locked those 500 records, the cursor will block when it tries to read one of those transactional protected records. The thread immediately stops operation at that point while the cursor waits for the read lock it has requested. Because that read lock will never be released (the thread can never make any forward progress), this represents a self-deadlock for the the thread.

There are three ways to prevent this self-deadlock:

1.  We can move the call to `countRecords()` to a point after the thread's transaction has committed.

2.  We can allow `countRecords()` to operate under the same transaction as all of the writes were performed (this is what the transaction parameter for the function is for).

3.  We can reduce our isolation guarantee for the application by allowing uncommitted reads.

For this example, we choose to use option 3 (uncommitted reads) to avoid the deadlock. This means that we have to open our database such that it supports uncommitted reads, and we have to open our cursor handle so that it knows to perform uncommitted reads.

Note that in <a href="inmem_txnexample_c.md" class="xref" title="In-Memory Transaction Example">In-Memory Transaction Example</a>, we simply perform the cursor operation using the same transaction as is used for the thread's writes.

The following is the `countRecords()` implementation. There is not anything particularly interesting about this function other than specifying uncommitted reads when we open the cursor handle, but we include the function here anyway for the sake of completeness.

``` c
// This simply counts the number of records contained in the
// database and returns the result.
//
// Note that this method exists only for illustrative purposes.
// A more straight-forward way to count the number of records in
// a database is to use the Database.getStats() method.
int
countRecords(Db *dbp, DbTxn *txn)
{

    Dbc *cursorp = NULL;
    int count = 0;

    try {
        // Get the cursor
        dbp->cursor(txn, &cursorp, DB_READ_UNCOMMITTED);

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

Finally, we provide the implementation of our `openDb()` function. This function should hold no surprises for you. Note, however, that we do specify uncommitted reads when we open the database. If we did not do this, then our `countRecords()` function would cause our thread to self-deadlock because the cursor could not be opened to support uncommitted reads (that flag on the cursor open would, in fact, be silently ignored by DB).

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
        openFlags = DB_CREATE              | // Allow database creation
                    DB_READ_UNCOMMITTED    | // Allow uncommitted reads
                    DB_AUTO_COMMIT         | /* Allow auto commit */
                    DB_THREAD;               /* Cause the database to
                                                be free-threaded */

        dbp->open(NULL,       // Txn pointer
                  fileName,   // File name
                  NULL,       // Logical db name
                  DB_BTREE,   // Database type (using btree)
                  openFlags,  // Open flags
                  0);         // File mode. Using defaults
    } catch (DbException &e) {
        std::cerr << progname << "open_db: db open failed:" << std::endl;
        std::cerr << e.what() << std::endl;
        return (EXIT_FAILURE);
    }

    return (EXIT_SUCCESS);
}  
```

This completes our transactional example. If you would like to experiment with this code, you can find the example in the following location in your DB distribution:

``` c
DB_INSTALL/examples_cxx/txn_guide
```
