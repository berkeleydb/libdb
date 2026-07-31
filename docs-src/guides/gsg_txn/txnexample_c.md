---
title: "Transaction Example"
api-name: "Transaction Example"
source: docs/gsg_txn/C/txnexample_c.html
---
## Transaction Example

The following code provides a fully functional example of a multi-threaded transactional DB application. For improved portability across platforms, this examples uses pthreads to provide threading support.

The example opens an environment and database and then creates 5 threads, each of which writes 500 records to the database. The keys used for these writes are pre-determined strings, while the data is a random value. This means that the actual data is arbitrary and therefore uninteresting; we picked it only because it requires minimum code to implement and therefore will stay out of the way of the main points of this example.

Each thread writes 10 records under a single transaction before committing and writing another 10 (this is repeated 50 times). At the end of each transaction, but before committing, each thread calls a function that uses a cursor to read every record in the database. We do this in order to make some points about database reads in a transactional environment.

Of course, each writer thread performs deadlock detection as described in this manual. In addition, normal recovery is performed when the environment is opened.

We start with our normal `include` directives:

``` c
/* File: txn_guide.c */

/* We assume an ANSI-compatible compiler */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <db.h>

#ifdef _WIN32
extern int getopt(int, char * const *, const char *);
#else
#include <unistd.h>
#endif 
```

We also need a directive that we use to identify how many threads we want our program to create:

``` c
/* Run 5 writers threads at a time.  */
#define NUMWRITERS 5 
```

Next we declare a couple of global variables (used by our threads), and we provide our forward declarations for the functions used by this example.

``` c
/*
 * Printing of pthread_t is implementation-specific, so we
 * create our own thread IDs for reporting purposes.
 */
int global_thread_num;
pthread_mutex_t thread_num_lock;

/* Forward declarations */
int count_records(DB *, DB_TXN *);
int open_db(DB **, const char *, const char *, DB_ENV *, u_int32_t);
int usage(void);
void *writer_thread(void *);  
```

We now implement our usage function, which identifies our only command line parameter:

``` c
/* Usage function */
int
usage()
{
    fprintf(stderr, " [-h <database_home_directory>]\n");
    return (EXIT_FAILURE); 
}  
```

With that, we have finished up our program's housekeeping, and we can now move on to the main part of our program. As usual, we begin with `main()`. First we declare all our variables, and then we initialize our DB handles.

``` c
int
main(int argc, char *argv[])
{
    /* Initialize our handles */
    DB *dbp = NULL;
    DB_ENV *envp = NULL; 

    pthread_t writer_threads[NUMWRITERS];
    int ch, i, ret, ret_t;
    u_int32_t env_flags;
    char *db_home_dir;
    /* Application name */
    const char *prog_name = "txn_guide";
    /* Database file name */
    const char *file_name = "mydb.db";  
```

Now we need to parse our command line. In this case, all we want is to know where our environment directory is. If the `-h` option is not provided when this example is run, the current working directory is used instead.

``` c
    /* Parse the command line arguments */
#ifdef _WIN32
    db_home_dir = ".\\";
#else
    db_home_dir = "./";
#endif
    while ((ch = getopt(argc, argv, "h:")) != EOF)
        switch (ch) {
        case 'h':
            db_home_dir = optarg;
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
    /* Create the environment */
    ret = db_env_create(&envp, 0);
    if (ret != 0) {
        fprintf(stderr, "Error creating environment handle: %s\n",
            db_strerror(ret));
        goto err;
    }

    env_flags =
      DB_CREATE     |  /* Create the environment if it does not exist */ 
      DB_RECOVER    |  /* Run normal recovery. */
      DB_INIT_LOCK  |  /* Initialize the locking subsystem */
      DB_INIT_LOG   |  /* Initialize the logging subsystem */
      DB_INIT_TXN   |  /* Initialize the transactional subsystem. This
                        * also turns on logging. */
      DB_INIT_MPOOL |  /* Initialize the memory pool (in-memory cache) */
      DB_THREAD;       /* Cause the environment to be free-threaded */  
```

Now we configure how we want deadlock detection performed. In our case, we will cause DB to perform deadlock detection by walking its internal lock tables looking for a block every time a lock is requested. Further, in the event of a deadlock, the thread that holds the youngest lock will receive the deadlock notification.

### Note

You will notice that every database operation checks the operation's status return code, and if an error (non-zero) status is returned, we log the error and then go to a `err` label in our program. Unlike object-oriented programs such as C++ or Java, we do not have `try` blocks in C. Therefore, this is the best way for us to implement cascading error handling for this example.

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
```

Now we open our environment.

``` c
    /*
     * If we had utility threads (for running checkpoints or 
     * deadlock detection, for example) we would spawn those
     * here. However, for a simple example such as this,
     * that is not required.
     */

    /* Now actually open the environment */
    ret = envp->open(envp, db_home_dir, env_flags, 0);
    if (ret != 0) {
        fprintf(stderr, "Error opening environment: %s\n",
            db_strerror(ret));
        goto err;
    } 
```

Now we call the function that will open our database for us. This is not very interesting, except that you will notice that we are specifying `DB_DUPSORT`. This is required purely by the data that we are writing to the database, and it is only necessary if you run the application more than once without first deleting the environment.

Also, we do not provide any error logging here because the `open_db()` function does that for us. (The implementation of `open_db()` is described later in this section.)

``` c
     /* Open the database */
    ret = open_db(&dbp, prog_name, file_name, envp, DB_DUPSORT);
    if (ret != 0)
        goto err;  
```

Now we create our threads. In this example we are using pthreads for our threading package. A description of threading (beyond how it impacts DB usage) is beyond the scope of this manual. However, the things that we are doing here should be familiar to anyone who has prior experience with any threading package. We are simply initializing a mutex, creating our threads, and then joining our threads, which causes our program to wait until the joined threads have completed before continuing operations in the main thread.

``` c
     /* Initialize a pthread mutex. Used to help provide thread ids. */
    (void)pthread_mutex_init(&thread_num_lock, NULL);

    /* Start the writer threads. */
    for (i = 0; i < NUMWRITERS; i++)
        (void)pthread_create(&writer_threads[i], NULL, 
            writer_thread, (void *)dbp);

    /* Join the writers */
    for (i = 0; i < NUMWRITERS; i++)
        (void)pthread_join(writer_threads[i], NULL);  
```

Finally, to wrap up `main()`, we close out our database and environment handle, as is normal for any DB application. Notice that this is where our `err` label is placed in our application. If any database operation prior to this point in the program returns an error status, the program simply jumps to this point and closes our handles if necessary before exiting the application completely.

``` c
err:
    /* Close our database handle, if it was opened. */
    if (dbp != NULL) {
        ret_t = dbp->close(dbp, 0);
        if (ret_t != 0) {
            fprintf(stderr, "%s database close failed: %s\n",
                file_name, db_strerror(ret_t));
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

Now that we have completed `main()`, we need to implement the function that our writer threads will actually run. This is where the bulk of our transactional code resides.

We start as usual with variable declarations and initialization.

``` c
/* 
 * A function that performs a series of writes to a
 * Berkeley DB database. The information written
 * to the database is largely nonsensical, but the
 * mechanisms of transactional commit/abort and
 * deadlock detection are illustrated here.
 */
void *
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
    DB_ENV *envp = dbp->get_env(dbp);  
```

Now we want a thread number for reporting purposes. It is possible to use the `pthread_t` value directly for this purpose, but how that is done unfortunately differs depending on the pthread implementation you are using. So instead we use a mutex-protected global variable to obtain a simple integer for our reporting purposes.

Note that we are also use this thread id for initializing a random number generator, which we do here. We use this random number generator for data generation.

``` c
    /* Get the thread number */
    (void)pthread_mutex_lock(&thread_num_lock);
    global_thread_num++;
    thread_num = global_thread_num;
    (void)pthread_mutex_unlock(&thread_num_lock); 

    /* Initialize the random number generator */
    srand((u_int)pthread_self());  
```

Now we begin the loop that we use to write data to the database. Notice that at the beginning of the top loop, we begin a new transaction. We will actually use 50 transactions per writer thread, although we will only ever have one active transaction per thread at a time. Within each transaction, we will perform 10 database writes.

By combining multiple writes together under a single transaction, we increase the likelihood that a deadlock will occur. Normally, you want to reduce the potential for a deadlock and in this case the way to do that is to perform a single write per transaction. To avoid deadlocks, we could be using auto commit to write to our database for this workload.

However, we want to show deadlock handling and by performing multiple writes per transaction we can actually observe deadlocks occurring. We also want to underscore the idea that you can combing multiple database operations together in a single atomic unit of work in order to improve the efficiency of your writes.

Finally, on an issue of style, you will notice the `retry` label that we place immediately before our transaction begin code. We use this to loop in the event that a deadlock is detected and the write operation has to be performed. A great many people dislike looping with `goto` statements, and we certainly could have written this code to avoid it. However, we find that using the `goto` in this case greatly helps to clarify the code, so we ignore the bias against `goto` programming in order to clearly support looping in the event of what is really an error condition.

``` c
    /* Write 50 times and then quit */
    for (i = 0; i < 50; i++) {
        retry_count = 0; /* Used for deadlock retries */

retry:
        /* Begin our transaction. */
        ret = envp->txn_begin(envp, NULL, &txn, 0);
        if (ret != 0) {
            envp->err(envp, ret, "txn_begin failed");
            return ((void *)EXIT_FAILURE);
        }  
```

Now we begin the inner loop that we use to actually perform the write. Notice that we use a `case` statement to examine the return code from the database put. This case statement is what we use to determine whether we need to abort (or abort/retry in the case of a deadlock) our current transaction.

``` c
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
                 * Our database is configured for sorted duplicates, 
                 * so there is a potential for a KEYEXIST error return. 
                 * If we get one, simply ignore it and continue on.
                 *
                 * Note that you will see KEYEXIST errors only after you
                 * have run this program at least once.
                 */
                case DB_KEYEXIST:
                    printf("Got keyexists.\n");
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
                     * First thing we MUST do is abort the 
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
                    return ((void *)EXIT_FAILURE);
                /* 
                 * If a generic error occurs, we simply abort the 
                 * transaction and exit the thread completely.
                 */
                default:
                    envp->err(envp, ret, "db put failed");
                    ret = txn->abort(txn);
                    if (ret != 0)
                        envp->err(envp, ret, "txn abort failed");
                    return ((void *)EXIT_FAILURE);
             } /** End case statement **/

        }   /** End for loop **/  
```

Having completed the inner database write loop, we could simply commit the transaction and continue on to the next block of 10 writes. However, we want to first illustrate a few points about transactional processing so instead we call our `count_records()` function before calling the transaction commit. `count_records()` uses a cursor to read every record in the database and return a count of the number of records that it found.

``` c
        /* 
         * print the number of records found in the database. 
         * See count_records() for usage information.
         */
        printf("Thread %i. Record count: %i\n", thread_num, 
            count_records(dbp, NULL));

        /* 
         * If all goes well, we can commit the transaction and
         * loop to the next transaction.
         */
        ret = txn->commit(txn, 0);
        if (ret != 0) {
            envp->err(envp, ret, "txn commit failed");
            return ((void *)EXIT_FAILURE);
        }
    }
    return ((void *)EXIT_SUCCESS);
}  
```

If you look at the `count_records()` function prototype at the beginning of this example, you will see that the function's second parameter takes a transaction handle. However, our usage of the function here does not pass a transaction handle through to the function.

Because `count_records()` reads every record in the database, if used incorrectly the thread will self-deadlock. The writer thread has just written 500 records to the database, but because the transaction used for that write has not yet been committed, each of those 500 records are still locked by the thread's transaction. If we then simply run a non-transactional cursor over the database from within the same thread that has locked those 500 records, the cursor will block when it tries to read one of those transactional protected records. The thread immediately stops operation at that point while the cursor waits for the read lock it has requested. Because that read lock will never be released (the thread can never make any forward progress), this represents a self-deadlock for the the thread.

There are three ways to prevent this self-deadlock:

1.  We can move the call to `count_records()` to a point after the thread's transaction has committed.

2.  We can allow `count_records()` to operate under the same transaction as all of the writes were performed (this is what the transaction parameter for the function is for).

3.  We can reduce our isolation guarantee for the application by allowing uncommitted reads.

For this example, we choose to use option 3 (uncommitted reads) to avoid the deadlock. This means that we have to open our database such that it supports uncommitted reads, and we have to open our cursor handle so that it knows to perform uncommitted reads.

Note that in <a href="inmem_txnexample_c.md" class="xref" title="In-Memory Transaction Example">In-Memory Transaction Example</a>, we simply perform the cursor operation using the same transaction as is used for the thread's writes.

The following is the `count_records()` implementation. There is not anything particularly interesting about this function other than specifying uncommitted reads when we open the cursor handle, but we include the function here anyway for the sake of completeness.

``` c
/* 
 * This simply counts the number of records contained in the
 * database and returns the result.
 *
 * Note that this function exists only for illustrative purposes.
 * A more straight-forward way to count the number of records in
 * a database is to use DB->stat() or DB->stat_print().
 */

int
count_records(DB *dbp, DB_TXN *txn)
{
    DBT key, value;
    DBC *cursorp;
    int count, ret;

    cursorp = NULL;
    count = 0;

    /* Get the cursor */
    ret = dbp->cursor(dbp, txn, &cursorp, DB_READ_UNCOMMITTED);
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
                dbp->err(dbp, ret, "Count records unspecified error");
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

Finally, we provide the implementation of our `open_db()` function. This function should hold no surprises for you. Note, however, that we do specify uncommitted reads when we open the database. If we did not do this, then our `count_records()` function would cause our thread to self-deadlock because the cursor could not be opened to support uncommitted reads (that flag on the cursor open would, in fact, be silently ignored by DB).

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
    open_flags = DB_CREATE              | /* Allow database creation */ 
                 DB_READ_UNCOMMITTED    | /* Allow uncommitted reads */
                 DB_AUTO_COMMIT         | /* Allow auto commit */
                 DB_THREAD;               /* Cause the database to be
                                             free-threaded */

    ret = dbp->open(dbp,        /* Pointer to the database */
                    NULL,       /* Txn pointer */
                    file_name,  /* File name */
                    NULL,       /* Logical db name */
                    DB_BTREE,   /* Database type (using btree) */
                    open_flags, /* Open flags */
                    0);         /* File mode. Using defaults */
    if (ret != 0) {
        dbp->err(dbp, ret, "Database '%s' open failed",
            file_name);
        return (EXIT_FAILURE);
    }
    return (EXIT_SUCCESS);
}  
```

This completes our transactional example. If you would like to experiment with this code, you can find the example in the following location in your DB distribution:

``` c
DB_INSTALL/examples_c/txn_guide
```
