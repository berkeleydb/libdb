---
title: "Program Listing"
api-name: "Program Listing"
source: docs/gsg_db_rep/JAVA/simpleprogramlisting.html
---
## Program Listing

<span class="sect2"> [Class: RepConfig](simpleprogramlisting.md#repconfiginfo_cxx) </span>

<span class="sect2"> [Class: SimpleTxn](simpleprogramlisting.md#simpletxnusage_java) </span>

<span class="sect2"> [Method: SimpleTxn.main()](simpleprogramlisting.md#simpletxnmain_java) </span>

<span class="sect2"> [Method: SimpleTxn.init()](simpleprogramlisting.md#simpletxn_init_java) </span>

<span class="sect2"> [Method: SimpleTxn.doloop()](simpleprogramlisting.md#doloop_java) </span>

<span class="sect2"> [Method: SimpleTxn.printStocks()](simpleprogramlisting.md#printstocks_c) </span>

Our example program is a fairly simple transactional application. At this early stage of its development, the application contains no hint that it must be network-aware so the only command line argument that it takes is one that allows us to specify the environment home directory. (Eventually, we will specify things like host names and ports from the command line).

Note that the application performs all writes under the protection of a transaction; however, multiple database operations are not performed per transaction. Consequently, we simplify things a bit by using autocommit for our database writes.

Also, this application is single-threaded. It is possible to write a multi-threaded or multi-process application that performs replication. That said, the concepts described in this book are applicable to both single threaded and multi-threaded applications so nothing is gained by multi-threading this application other than distracting complexity. This manual does, however, identify where care must be taken when performing replication with a non-single threaded application.

Finally, remember that transaction processing is not described in this manual. Rather, see the *Berkeley DB Getting Started with Transaction Processing* guide for details on that topic.

### Class: RepConfig

Before we begin, we present a class that we will use to maintain useful information for us. Under normal circumstances, this class would not be necessary for a simple transactional example such as this. However, this code will grow into a replicated example that needs to track a lot more information for the application, and so we lay the groundwork for it here.

The class that we create is called `RepConfig` and its only purpose at this time is to track the location of our environment home directory.

``` c
package db.repquote_gsg;

public class RepConfig
{
    // Constant values used in the RepQuote application.
    public static final String progname = "SimpleTxn";
    public static final int CACHESIZE = 10 * 1024 * 1024;

    // member variables containing configuration information
    public String home; // String specifying the home directory for 
                        // rep files.

    public RepConfig()
    {
        home = "TESTDIR";
    }

    public java.io.File getHome()
    {
        return new java.io.File(home);
    }

}  
```

### Class: SimpleTxn

Our transactional example will consist of a class, `SimpleTxn`, that performs all our work for us.

First, we provide the package declaration and then a few import statements that the class needs.

``` c
package db.repquote_gsg;

import java.io.FileNotFoundException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.UnsupportedEncodingException;

import com.sleepycat.db.Cursor;
import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseConfig;
import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.DatabaseType;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;
import com.sleepycat.db.LockMode;
import com.sleepycat.db.OperationStatus;
import db.repquote_gsg.RepConfig;

public class SimpleTxn
{
    private RepConfig repConfig;
    private Environment dbenv; 
```

Next, we provide our class constructor. This simply initializes our class data members.

``` c
    public SimpleTxn()
        throws DatabaseException
    {
        repConfig = null;
        dbenv = null;
    }  
```

And then we provide our `usage()` method. At this point, this method has very little to report:

``` c
    public static void usage()
    {
        System.err.println("usage: " + repConfig.progname);
        System.err.println("-h home");

        System.err.println("\t -h home directory\n");

        System.exit(1);
    }  
```

### Method: SimpleTxn.main()

Having implemented our `usage()` method, we can jump directly into our `main()` method. This method begins by instantiating a `RepConfig` object, and then collecting the command line arguments so that it can populate the object with the appropriate data (just the environment home directory, at this time):

``` c
    public static void main(String[] argv)
        throws Exception
    {
        RepConfig config = new RepConfig();
        // Extract the command line parameters
        for (int i = 0; i < argv.length; i++)
        {
            if (argv[i].compareTo("-h") == 0) {
                // home - a string arg.
                i++;
                config.home = argv[i];
            } else {
                System.err.println("Unrecognized option: " + argv[i]);
                usage();
            }
        }  
```

And then perform a little sanity checking on the command line input:

``` c
        // Error check command line.
        if (config.home.length() == 0)
            usage();  
```

Now we perform the class' work. To begin, we initialize the object. The `init()` method actually opens our environment for us (shown in the next section).

``` c
        SimpleTxn runner = null;
        try {
            runner = new SimpleTxn();
            runner.init(config);  
```

And then we call our `doloop()` method. This method is where we perform all our database activity. See <a href="simpleprogramlisting.md#doloop_java" class="xref" title="Method: SimpleTxn.doloop()">Method: SimpleTxn.doloop()</a> for it's details.

``` c
                    runner.doloop();  
```

And then, finally terminate the application (which closes our environment handle) and end the method.

``` c
            runner.terminate();
        } catch (DatabaseException dbe) {
            System.err.println("Caught an exception during " +
                "initialization or processing: " + dbe.toString());
            if (runner != null)
                runner.terminate();
        }
            System.exit(0);
    } // end main  
```

### Method: SimpleTxn.init()

The `SimpleTxn.init()` method is used to open our environment handle. For readers familiar with writing transactional DB applications, there should be no surprises here. However, we will be adding to this in later chapters as we roll replication into this example.

The only thing worth noting in this method here is that we relax our transactional durability guarantee for this application. We do this because the application will eventually be replicated and so we don't need a high durability guarantee.

``` c
    public int init(RepConfig config)
        throws DatabaseException
    {
        int ret = 0;
        repConfig = config;
        EnvironmentConfig envConfig = new EnvironmentConfig();
        envConfig.setErrorStream(System.err);
        envConfig.setErrorPrefix(RepConfig.progname);

        envConfig.setCacheSize(RepConfig.CACHESIZE);
        envConfig.setTxnNoSync(true);

        envConfig.setAllowCreate(true);
        envConfig.setRunRecovery(true);
        envConfig.setInitializeLocking(true);
        envConfig.setInitializeLogging(true);
        envConfig.setInitializeCache(true);
        envConfig.setTransactional(true);
        try {
            dbenv = new Environment(repConfig.getHome(), envConfig);
        } catch(FileNotFoundException e) {
            System.err.println("FileNotFound exception: " + e.toString());
            System.err.println(
                "Ensure that the environment directory is pre-created.");
            ret = 1;
        }

        return ret;
    }  
```

Finally, we present the `SimpleTxn.terminate()` method here. All this does is close the environment handle. Again, there should be no surprises here, but we provide the implementation for the sake of completeness anyway.

``` c
    public void terminate()
        throws DatabaseException
    {
            dbenv.close();
    }  
```

### Method: SimpleTxn.doloop()

We now implement our application's primary data processing method. This method provides a command prompt at which the user can enter a stock ticker value and a price for that value. This information is then entered to the database.

To display the database, simply enter `return` at the prompt.

To begin, we declare a database pointer:

``` c
    public int doloop()
        throws DatabaseException, , UnsupportedEncodingException
    {
        Database db = null;  
```

Next, we begin the loop and we immediately open our database if it has not already been opened.

``` c
        for (;;)
        {
            if (db == null) {
                DatabaseConfig dbconf = new DatabaseConfig();
                dbconf.setType(DatabaseType.BTREE);
                dbconf.setAllowCreate(true);
                dbconf.setTransactional(true);

                try {
                    db = dbenv.openDatabase(null,           // Txn handle
                                        RepConfig.progname, // db filename
                                        null,               // db name
                                        dbconf);
               } catch (FileNotFoundException fnfe) {
                    System.err.println("File not found exception" + 
                                        fnfe.toString());
                    // Get here only if the environment home directory
                    // somehow does not exist.
               }
            }  
```

Now we implement our command prompt. This is a simple and not very robust implementation of a command prompt. If the user enters the keywords `exit` or `quit`, the loop is exited and the application ends. If the user enters nothing and instead simply presses `return`, the entire contents of the database is displayed. We use our `printStocks()` method to display the database. (That implementation is shown next in this chapter.)

Notice that very little error checking is performed on the data entered at this prompt. If the user fails to enter at least one space in the value string, a simple help message is printed and the prompt is returned to the user. That is the only error checking performed here. In a real-world application, at a minimum the application would probably check to ensure that the price was in fact an integer or float value. However, in order to keep this example code as simple as possible, we refrain from implementing a thorough user interface.

``` c
            BufferedReader stdin =
                new BufferedReader(new InputStreamReader(System.in));

            // listen for input, and add it to the database.
            System.out.print("QUOTESERVER> ");
            System.out.flush();
            String nextline = null;
            try {
                nextline = stdin.readLine();
            } catch (IOException ioe) {
                System.err.println("Unable to get data from stdin");
                break;
            }
            String[] words = nextline.split("\\s");

            // A blank line causes the DB to be dumped to stdout.
            if (words.length == 0 ||
                (words.length == 1 && words[0].length() == 0)) {
                try {
                    printStocks(db);
                } catch (DatabaseException e) {
                    System.err.println("Got db exception reading " +
                        "DB: " + e.toString());
                    break;
                }
                continue;
            }

            if (words.length == 1 &&
                (words[0].compareToIgnoreCase("quit") == 0 ||
                words[0].compareToIgnoreCase("exit") == 0)) {
                break;
            } else if (words.length != 2) {
                System.err.println("Format: TICKER VALUE");
                continue;
            }  
```

Now we assign data to the `DatabaseEntry` classes that we will use to write the new information to the database.

``` c
            DatabaseEntry key =
                    new DatabaseEntry(words[0].getBytes("UTF-8"));
            DatabaseEntry data =
                    new DatabaseEntry(words[1].getBytes("UTF-8"));  
```

Having done that, we can write the new information to the database. Remember that because a transaction handle is not explicitly used, but we did open the database such that it supports transactions, then autocommit is automatically used for this database write.

Autocommit is described in the *Berkeley DB Getting Started with Transaction Processing* guide.

Also, the database is not configured for duplicate records, so the data portion of a record is overwritten if the provided key already exists in the database. However, in this case DB returns `OperationStatus.KEYEXIST` — which we ignore.

``` c
            db.put(null, key, data);  
```

Finally, we close our database before returning from the method.

``` c
        }
        if (db != null)
            db.close(true);
        return 0;
    }  
```

### Method: SimpleTxn.printStocks()

The `printStocks()` method simply takes a database handle, opens a cursor, and uses it to display all the information it finds in a database. This is trivial cursor operation that should hold no surprises for you. We simply provide it here for the sake of completeness.

If you are unfamiliar with basic cursor operations, please see the *Getting Started with Berkeley DB* guide.

``` c
    public void terminate()
        throws DatabaseException
    {
            dbenv.close();
    }

    /*
     * void return type since error conditions are propagated
     * via exceptions.
     */
    private void printStocks(Database db)
        throws DatabaseException
    {
        Cursor dbc = db.openCursor(null, null);

        System.out.println("\tSymbol\tPrice");
        System.out.println("\t======\t=====");

        DatabaseEntry key = new DatabaseEntry();
        DatabaseEntry data = new DatabaseEntry();
        OperationStatus ret;
        for (ret = dbc.getFirst(key, data, LockMode.DEFAULT);
            ret == OperationStatus.SUCCESS;
            ret = dbc.getNext(key, data, LockMode.DEFAULT)) {
            String keystr = new String
                (key.getData(), key.getOffset(), key.getSize());
            String datastr = new String
                (data.getData(), data.getOffset(), data.getSize());
            System.out.println("\t"+keystr+"\t"+datastr);

        }
        dbc.close();
    }
} // end class  
```
