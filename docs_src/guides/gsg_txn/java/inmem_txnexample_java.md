---
title: "Base API In-Memory Transaction Example"
api-name: "Base API In-Memory Transaction Example"
source: docs/gsg_txn/JAVA/inmem_txnexample_java.html
---
## Base API In-Memory Transaction Example

DB is sometimes used for applications that simply need to cache data retrieved from some other location (such as a remote database server). DB is also often used in embedded systems.

In both cases, applications may still want to use transactions for atomicity, consistency, and isolation guarantees, but they may want to forgo the durability guarantee entirely. That is, they may want their DB environment and databases kept entirely in-memory so as to avoid the performance impact of unneeded disk I/O.

To do this:

- Refrain from specifying a home directory when you open your environment. The exception to this is if you are using the `DB_CONFIG` configuration file — in that case you must identify the environment's home directory so that the configuration file can be found.

- Configure your environment to back your regions from system memory instead of the filesystem.

- Configure your logging subsystem such that log files are kept entirely in-memory.

- Increase the size of your in-memory log buffer so that it is large enough to hold the largest set of concurrent write operations.

- Increase the size of your in-memory cache so that it can hold your entire data set. You do not want your cache to page to disk.

- Do not specify a file name when you open your database(s).

As an example, this section takes the transaction example provided in <a href="txnexample_java.md" class="xref" title="Base API Transaction Example">Base API Transaction Example</a> and it updates that example so that the environment, database, log files, and regions are all kept entirely in-memory.

For illustration purposes, we also modify this example so that uncommitted reads are no longer used to enable the `countRecords()` method. Instead, we simply provide a transaction handle to `countRecords()` so as to avoid the self-deadlock.

The majority of the modifications to the original example are performed in the `TxnGuide` example class (see <a href="txnexample_java.md#txnguideexample" class="xref" title="TxnGuide.java">TxnGuide.java</a>). This is because the majority of the work that we need to do is performed when the environment and databases are opened.

To begin, we simplify the beginning of the class a bit. We eliminate some variables that the example no longer needs — specifically variables having to do with the location of the environment and the names of the database files. We can also remove our `usage()` method because we no longer require any command line arguments.

``` c
// File TxnGuideInMemory.java

package db.txn;

import com.sleepycat.bind.serial.StoredClassCatalog;

import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseConfig;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.DatabaseType;
import com.sleepycat.db.LockDetectMode;

import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;

import java.io.File;
import java.io.FileNotFoundException;

public class TxnGuideInMemory {

    // DB handles
    private static Database myDb = null;
    private static Database myClassDb = null;
    private static Environment myEnv = null;

    private static final int NUMTHREADS = 5; 
```

Next, in our `main()` method, we remove the call to `parseArgs()` because that only existed in the previous example for collecting the environment home location. Everything else is essentially the same.

``` c
    public static void main(String args[]) {
        try {

            // Open the environment and databases
            openEnv();

            // Get our class catalog (used to serialize objects)
            StoredClassCatalog classCatalog =
                new StoredClassCatalog(myClassDb);

            // Start the threads
            DBWriter[] threadArray;
            threadArray = new DBWriter[NUMTHREADS];
            for (int i = 0; i < NUMTHREADS; i++) {
                threadArray[i] = new DBWriter(myEnv, myDb, classCatalog);
                threadArray[i].start();
            }

            for (int i = 0; i < NUMTHREADS; i++) {
                threadArray[i].join();
            }
        } catch (Exception e) {
            System.err.println("TxnGuideInMemory: " + e.toString());
            e.printStackTrace();
        } finally {
            closeEnv();
        }
        System.out.println("All done.");
    } 
```

Next we open our environment as always. However, in doing so we:

- Set `EnvironmentConfig.setPrivate()` to `true`. This causes our environment to back regions using our application's heap memory rather than by using the filesystem. This is the first important step to keeping our DB data entirely in-memory.

- Remove `runRecovery()` from the environment configuration. Because all our data will be held entirely in memory, recovery is a non-issue. Note that if we had left the call to `runRecovery()` in, it would be silently ignored.

``` c
    private static void openEnv() throws DatabaseException {
        System.out.println("opening env");

        // Set up the environment.
        EnvironmentConfig myEnvConfig = new EnvironmentConfig();

        // Region files are not backed by the filesystem, they are
        // backed by heap memory.
        myEnvConfig.setPrivate(true);

        myEnvConfig.setAllowCreate(true);
        myEnvConfig.setInitializeCache(true);
        myEnvConfig.setInitializeLocking(true);
        myEnvConfig.setInitializeLogging(true);
        myEnvConfig.setTransactional(true);
        // EnvironmentConfig.setThreaded(true) is the default behavior 
        // in Java, so we do not have to do anything to cause the
        // environment handle to be free-threaded.

        // Indicate that we want db to internally perform deadlock 
        // detection. Also indicate that the transaction that has
        // performed the least amount of write activity to
        // receive the deadlock notification, if any.
        myEnvConfig.setLockDetectMode(LockDetectMode.MINWRITE); 
```

Now we configure our environment to keep the log files in memory, increase the log buffer size to 10 MB, and increase our in-memory cache to 10 MB. These values should be more than enough for our application's workload.

``` c
        
                  // Specify in-memory logging
        myEnvConfig.setLogInMemory(true);
        // Specify the size of the in-memory log buffer
        // Must be large enough to handle the log data created by
        // the largest transaction.
        myEnvConfig.setLogBufferSize(10 * 1024 * 1024);
        // Specify the size of the in-memory cache
        // Set it large enough so that it won't page.
        myEnvConfig.setCacheSize(10 * 1024 * 1024); 
        
      
```

Our database configuration is identical to the original example, except that we do not specify `setReadUncomitted()` here. We will be causing our `countRecords()` method to join the transaction rather than perform uncommitted reads, so we do not need our database to support them.

``` c
        // Set up the database
        DatabaseConfig myDbConfig = new DatabaseConfig();
        myDbConfig.setType(DatabaseType.BTREE);
        myDbConfig.setAllowCreate(true);
        myDbConfig.setTransactional(true);
        myDbConfig.setSortedDuplicates(true);
        // no DatabaseConfig.setThreaded() method available.
        // db handles in java are free-threaded so long as the
        // env is also free-threaded.  
```

Next, we open the environment. This is identical to how the example previously worked, except that we do not provide a location for the environment's home directory.

``` c
        try {
            // Open the environment
            myEnv = new Environment(null,    // Env home
                                    myEnvConfig); 
```

When we open our databases, we also specify `null` for the file names. The causes the database to not be backed by the filesystem; that is, the databases are held entirely in memory.

``` c
            // Open the database. Do not provide a txn handle. This open
            // is auto committed because DatabaseConfig.setTransactional()
            // is true.
            myDb = myEnv.openDatabase(null,     // txn handle
                                      null,     // Database file name
                                      null,     // Database name
                                      myDbConfig);

            // Used by the bind API for serializing objects 
            // Class database must not support duplicates
            myDbConfig.setSortedDuplicates(false);
            myClassDb = myEnv.openDatabase(null,     // txn handle
                                           null,     // Database file name
                                           null,     // Database name,
                                           myDbConfig);
        } catch (FileNotFoundException fnfe) {
            System.err.println("openEnv: " + fnfe.toString());
            System.exit(-1);
        }
    } 
```

After that, our class is unchanged, except for some very minor modifications. Most notably, we remove the `parseArgs()` method from the application, because we no longer need it.

``` c
    private static void closeEnv() {
        System.out.println("Closing env");
        if (myDb != null ) {
            try {
                myDb.close();
            } catch (DatabaseException e) {
                System.err.println("closeEnv: myDb: " +
                    e.toString());
                e.printStackTrace();
            }
        }

        if (myClassDb != null ) {
            try {
                myClassDb.close();
            } catch (DatabaseException e) {
                System.err.println("closeEnv: myClassDb: " +
                    e.toString());
                e.printStackTrace();
            }
        }

        if (myEnv != null ) {
            try {
                myEnv.close();
            } catch (DatabaseException e) {
                System.err.println("closeEnv: " + e.toString());
                e.printStackTrace();
            }
        }
    }

    private TxnGuideInMemory() {}
} 
```

That completes our modifications to this class. We now turn our attention to our `DBWriter` class (see <a href="txnexample_java.md#dbwriter" class="xref" title="DBWriter.java">DBWriter.java</a>). It is unchanged, except for one small modification. In the `run()` method, we call `countRecords()` with a transaction handle, rather than configuring our entire application for uncommitted reads. Both mechanisms work well-enough for preventing a self-deadlock. However, the individual count in this example will tend to be lower than the counts seen in the previous transaction example, because `countRecords()` can no longer see records created but not yet committed by other threads. Additionally, the usage of the transaction handle here will probably cause more deadlocks than using read-uncommitted does, because more locking is being performed in this case.

``` c
package db.txn;

import com.sleepycat.bind.EntryBinding;
import com.sleepycat.bind.serial.StoredClassCatalog;
import com.sleepycat.bind.serial.SerialBinding;
import com.sleepycat.bind.tuple.StringBinding;

import com.sleepycat.db.Cursor;
import com.sleepycat.db.CursorConfig;
import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.DeadlockException;
import com.sleepycat.db.Environment;
import com.sleepycat.db.LockMode;
import com.sleepycat.db.OperationStatus;
import com.sleepycat.db.Transaction;

import java.io.UnsupportedEncodingException;
import java.util.Random;

public class DBWriter extends Thread
{
    private Database myDb = null;
    private Environment myEnv = null;
    private EntryBinding dataBinding = null;
    private Random generator = new Random();

    private static final int MAX_RETRY = 20;

    private static String[] keys = {"key 1", "key 2", "key 3",
                                    "key 4", "key 5", "key 6",
                                    "key 7", "key 8", "key 9",
                                    "key 10"};

    // Constructor. Get our DB handles from here
    DBWriter(Environment env, Database db, StoredClassCatalog scc)
        throws DatabaseException {
        myDb = db;
        myEnv = env;
        dataBinding = new SerialBinding(scc, PayloadData.class);
    }

    // Thread method that writes a series of records
    // to the database using transaction protection.
    // Deadlock handling is demonstrated here.
    public void run () {
        Transaction txn = null;

        // Perform 50 transactions
        for (int i=0; i<50; i++) {

           boolean retry = true;
           int retry_count = 0;
           // while loop is used for deadlock retries
           while (retry) {
                // try block used for deadlock detection and
                // general db exception handling
                try {

                    // Get a transaction
                    txn = myEnv.beginTransaction(null, null);
                    // Write 10 records to the db
                    // for each transaction
                    for (int j = 0; j < 10; j++) {
                        // Get the key
                        DatabaseEntry key = new DatabaseEntry();
                        StringBinding.stringToEntry(keys[j], key);

                        // Get the data
                        PayloadData pd = new PayloadData(i+j, getName(),
                            generator.nextDouble());
                        DatabaseEntry data = new DatabaseEntry();
                        dataBinding.objectToEntry(pd, data);

                        // Do the put
                        myDb.put(txn, key, data);
                    }

                    // commit
                    System.out.println(getName() + 
                        " : committing txn : " + i);

                    System.out.println(getName() + " : Found " +
                        countRecords(txn) + " records in the database.");
                    try {
                        txn.commit();
                        txn = null;
                    } catch (DatabaseException e) {
                        System.err.println("Error on txn commit: " +
                            e.toString());
                    }
                    retry = false;

                } catch (DeadlockException de) {
                    System.out.println("################# " + getName() +
                        " : caught deadlock");
                    // retry if necessary
                    if (retry_count < MAX_RETRY) {
                        System.err.println(getName() +
                            " : Retrying operation.");
                        retry = true;
                        retry_count++;
                    } else {
                        System.err.println(getName() +
                            " : out of retries. Giving up.");
                        retry = false;
                    }
                } catch (DatabaseException e) {
                    // abort and don't retry
                    retry = false;
                    System.err.println(getName() +
                        " : caught exception: " + e.toString());
                    System.err.println(getName() +
                        " : errno: " + e.getErrno());
                    e.printStackTrace();
                } finally {
                    if (txn != null) {
                        try {
                            txn.abort();
                        } catch (Exception e) {
                            System.err.println(
                                "Error aborting transaction: " + 
                                e.toString());
                            e.printStackTrace();
                        }
                    }
                }
            }
        }
    } 
```

Next we update `countRecords()`. The only difference here is that we no longer specify `CursorConfig.setReadUncomitted()` when we open our cursor. Note that even this minor change is not required. If we do not configure our database to support uncommitted reads, `CursorConfig.setReadUncomitted()` is silently ignored. However, we remove the property anyway from the cursor open so as to avoid confusion.

``` c
    // This simply counts the number of records contained in the
    // database and returns the result. You can use this method
    // in three ways:
    //
    // First call it with an active txn handle.
    // Secondly, configure the cursor for uncommitted reads
    // Third, call count_records AFTER the writer has committed
    //    its transaction.
    //
    // If you do none of these things, the writer thread will 
    // self-deadlock.
    //
    // Note that this method exists only for illustrative purposes.
    // A more straight-forward way to count the number of records in
    // a database is to use the Database.getStats() method.
    private int countRecords(Transaction txn)  throws DatabaseException {
        DatabaseEntry key = new DatabaseEntry();
        DatabaseEntry data = new DatabaseEntry();
        int count = 0;
        Cursor cursor = null;

        try {
            // Get the cursor
            CursorConfig cc = new CursorConfig();
            cc.setReadUncomitted(true);
            cursor = myDb.openCursor(txn, cc);
            while (cursor.getNext(key, data, LockMode.DEFAULT) ==
                    OperationStatus.SUCCESS) {

                    count++;
            }
        } finally {
            if (cursor != null) {
                cursor.close();
            }
        }

        return count;

    }
} 
```

This completes our in-memory transactional example. If you would like to experiment with this code, you can find the example in the following location in your DB distribution:

``` c
DB_INSTALL/examples_java/src/db/txn
```
