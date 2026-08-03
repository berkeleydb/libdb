---
title: "Example Processing Loop"
api-name: "Example Processing Loop"
source: docs/gsg_db_rep/JAVA/exampledoloop.html
---
## Example Processing Loop

<span class="sect2"> [Running It](exampledoloop.md#runningit) </span>

In this section we take the example processing loop that we presented in the previous section and we flesh it out to provide a more complete example. We do this by updating the `doloop()` function that our original transaction application used (see <a href="simpleprogramlisting.md#doloop_java" class="xref" title="Method: SimpleTxn.doloop()">Method: SimpleTxn.doloop()</a>) to fully support our replicated application.

In the following example code, code that we add to the original example is presented in **`bold`**.

To begin, we must implement a way to track whether our application is running as a master or a client. There are many ways to do this, but in this case what we will do is extend `com.sleepycat.db.Environment` to carry the information. We do this by creating the `RepQuoteEnvironment` class.

``` c
package db.repquote;

import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;

public class RepQuoteEnvironment extends Environment
{
    private boolean isMaster;

    public RepQuoteEnvironment(final java.io.File host,
        EnvironmentConfig config)
        throws DatabaseException, java.io.FileNotFoundException
    {
        super(host, config);
        isMaster = false;
    }

    boolean getIsMaster()
    {
        return isMaster;
    }

    public void setIsMaster(boolean isMaster)
    {
        this.isMaster = isMaster;
    }
}
```

Next, we go to `RepQuoteExampleGSG.java` and we include the `RepQuoteEnvironment` class as well as the `EventHandler` class. We then cause our `RepQuoteExampleGSG` class to implement `EventHandler`. We also change our environment handle to be an instance of `RepQuoteEnvironment` instead of `Environment`.

Note that we also import the `com.sleepycat.db.ReplicationHandleDeadException` class. We will discuss what that exception is used for a little later in this example.

``` c
package db.repquote;

import java.io.FileNotFoundException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.lang.Thread;
import java.lang.InterruptedException;

import com.sleepycat.db.Cursor;
import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseConfig;
import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.DatabaseType;
import com.sleepycat.db.EnvironmentConfig;
import com.sleepycat.db.EventHandler;
import com.sleepycat.db.LockMode;
import com.sleepycat.db.OperationStatus;
import com.sleepycat.db.ReplicationHandleDeadException;
import com.sleepycat.db.ReplicationHostAddress;
import com.sleepycat.db.ReplicationManagerSiteConfig;

import db.repquote.RepConfig;
import db.repquote.RepQuoteEnvironment

public class RepQuoteExampleGSG  implements EventHandler
{
    private RepConfig repConfig;
    private RepQuoteEnvironment dbenv; 
```

That done, we can skip the `main()` method and our class constructor, because they do not change. Instead, we skip down to our `init()` method where we take care of opening our environment and setting the event handler.

To update our `init()` method, we only need to do a couple of things. First, we identify the current class as the event handler. Then, when we open our environment, we instantiate a `RepQuoteEnvironment` class instead of an `Environment` class.

``` c
    public int init(RepConfig config)
        throws DatabaseException
    {
        int ret = 0;
        repConfig = config;
        EnvironmentConfig envConfig = new EnvironmentConfig();
        envConfig.setErrorStream(System.err);
        envConfig.setErrorPrefix(RepConfig.progname);

        envConfig.addReplicationManagerSite(repConfig.getThisHost());
        for (ReplicationHostAddress host = 
            repConfig.getFirstOtherHost(); host != null; 
            host = repConfig.getNextOtherHost()){

            ReplicationManagerSiteConfig repmgrRemoteSiteConfig =
                new ReplicationManagerSiteConfig(host.host, host.port);
            repmgrRemoteSiteConfig.setBootstrapHelper(true);
            envConfig.addReplicationManagerSite(
                repmgrRemoteSiteConfig);
        }

        envConfig.setReplicationPriority(repConfig.priority);

        envConfig.setReplicationManagerAckPolicy(
            ReplicationManagerAckPolicy.ALL);
        envConfig.setCacheSize(RepConfig.CACHESIZE);
        envConfig.setTxnNoSync(true);

        envConfig.setEventHandler(this);

        envConfig.setAllowCreate(true);
        envConfig.setRunRecovery(true);
        envConfig.setThreaded(true);
        envConfig.setInitializeReplication(true);
        envConfig.setInitializeLocking(true);
        envConfig.setInitializeLogging(true);
        envConfig.setInitializeCache(true);
        envConfig.setTransactional(true);
        envConfig.setVerboseReplication(appConfig.verbose);
        try {
            dbenv = new RepQuoteEnvironment(repConfig.getHome(), 
                                                       envConfig);
        } catch(FileNotFoundException e) {
            System.err.println("FileNotFound exception: " + e.toString());
            System.err.println(
                "Ensure that the environment directory is pre-created.");
            ret = 1;
        }

        // start Replication Manager
        dbenv.replicationManagerStart(3, repConfig.startPolicy);
        return ret;
    } 
```

That done, we need to implement the methods required for responding to replication events. These methods are required because we are now implementing `com.sleepycat.db.EventHandler`. While we are required to provide an implementation for all of these methods, for our simple application we are really only interested in these because they allow us to track whether a replication instance is a master.

``` c
    public void handleRepClientEvent()
    {
        dbenv.setIsMaster(false);
    }

    public void handleRepConnectBrokenEvent()
    {
        // Ignored for now.
    }

    public void handleRepConnectEstablishedEvent()
    {
        // Ignored for now.
    }

    public void handleRepConnectTryFailedEvent()
    {
        // Ignored for now.
    }

    public void handleRepMasterEvent()
    {
        dbenv.setIsMaster(true);
    }

    public void handleRepNewMasterEvent(int envId)
    {
        // Ignored for now.
    }

    public void handleWriteFailedEvent(int errorCode)
    {
        System.err.println("Write to stable storage failed!" +
            "Operating system error code:" + errorCode);
        System.err.println("Continuing....");
    }

    public void handleRepStartupDoneEvent()
    {
        // Ignored for now.
    }

    public void handleRepPermFailedEvent()
    {
    // Ignored for now.
    }

    public void handleRepLocalSiteRemovedEvent()
    {
        // Ignored for now.
    }

    public void handleRepSiteAddedEvent()
    {
        // Ignored for now.
    }

    public void handleRepSiteRemovedEvent()
    {
        // Ignored for now.
    }

    public void handleRepElectedEvent()
    {
        // Safely ignored for Replication Manager applications.
    }

    public void handleRepElectionFailedEvent()
    {
        // Safely ignored for Replication Manager applications that do
        // not manage their own master selection.
    }

    public void handleRepJoinFailureEvent()
    {
        // Safely ignored since this application did not turn off AUTOINIT.
    }

    public void handleRepMasterFailureEvent()
    {
        // Safely ignored for Replication Manager applications that do
        // not manage their own master selection.
    }

    public void handleRepDupmasterEvent()
    {
        // Safely ignored for Replication Manager applications that do
        // not manage their own master selection.
    }

    public void handlePanicEvent()
    {
        System.err.println("Panic encountered!");
        System.err.println("Shutting down.");
        System.err.println("You should restart, running recovery.");
        try {
            terminate();
        } catch (DatabaseException dbe) {
            System.err.println("Caught an exception during " +
                "termination in handlePanicEvent: " + dbe.toString());
        }
        System.exit(-1);
    } 
```

That done, we need to update our `doloop()` method.

We begin by updating our `DatabaseConfig` instance to determine which options to use, depending on whether the application is running as a master.

``` c
    public int doloop()
        throws DatabaseException
    {
        Database db = null;

        for (;;)
        {
            if (db == null) {
                DatabaseConfig dbconf = new DatabaseConfig();
                dbconf.setType(DatabaseType.BTREE);
                if (dbenv.getIsMaster()) {
                    dbconf.setAllowCreate(true);
                }
                dbconf.setTransactional(true); 
```

When we open the database, we modify our error handling to account for the case where the database does not yet exist. This can happen if our code is running as a replica and the Replication Manager has not yet had a chance to create the databases for us. Recall that replicas never write to their own databases directly, and so they cannot create databases on their own.

If we detect that the database does not yet exist, we simply close the database handle, sleep for a short period of time and then continue processing. This gives the Replication Manager a chance to create the database so that our replica can continue operations.

``` c
                try {
                    db = dbenv.openDatabase
                        (null, RepConfig.progname, null, dbconf);
                } catch (java.io.FileNotFoundException e) {
                    System.err.println("no stock database available yet.");
                    if (db != null) {
                        db.close(true);
                        db = null;
                    }
                    try {
                        Thread.sleep(RepConfig.SLEEPTIME);
                    } catch (InterruptedException ie) {}
                    continue;
                }
            } 
```

Next we modify our prompt, so that if the local process is running as a replica, we can tell from the shell that the prompt is for a read-only process.

``` c
            BufferedReader stdin =
                new BufferedReader(new InputStreamReader(System.in));

            // listen for input, and add it to the database.
            System.out.print("QUOTESERVER");
            if (!dbenv.getIsMaster())
                System.out.print("(read-only)");
            System.out.print("> ");
            System.out.flush();
            String nextline = null;
            try {
                nextline = stdin.readLine();
            } catch (IOException ioe) {
                System.err.println("Unable to get data from stdin");
                break;
            }
            String[] words = nextline.split("\\s"); 
```

When we collect data from the prompt, there is a case that says if no data is entered then show the entire stocks database. This display is performed by our `print_stocks()` method (which has not required a modification since we first introduced it in <a href="simpleprogramlisting.md#printstocks_c" class="xref" title="Method: SimpleTxn.printStocks()"><span>Method: SimpleTxn.printStocks()</span></a> ).

When we call `printStocks()`, we check for a dead replication handle. Dead replication handles happen whenever a replication election results in a previously committed transaction becoming invalid. This is an error scenario caused by a new master having a slightly older version of the data than the original master and so all replicas must modify their database(s) to reflect that of the new master. In this situation, some number of previously committed transactions may have to be unrolled. From the replica's perspective, the database handles should all be closed and then opened again.

``` c
            // A blank line causes the DB to be dumped to stdout.
            if (words.length == 0 ||
                (words.length == 1 && words[0].length() == 0)) {
                try {
                    printStocks(db);
                } catch (DeadlockException de) {
                    continue;
                // Dead replication handles are caused by an election
                // resulting in a previously committing read becoming
                // invalid. Close the db handle and reopen.
                } catch (ReplicationHandleDeadException rhde) {
                    db.close(true); // close no sync.
                    db = null;
                    continue;
                } catch (DatabaseException e) {
                    System.err.println("Got db exception reading " +
                        "replication DB: " + e.toString());
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

That done, we need to add a little error checking to our command prompt to make sure the user is not attempting to modify the database at a replica. Remember, replicas must never modify their local databases on their own. This guards against that happening due to user input at the prompt.

``` c
            if (!dbenv.getIsMaster()) {
                System.err.println("Can't update client.");
                continue;
            }

            DatabaseEntry key = new DatabaseEntry(words[0].getBytes());
            DatabaseEntry data = new DatabaseEntry(words[1].getBytes());

            db.put(null, key, data);
        }
        if (db != null)
            db.close(true);
        return 0;
    } 
```

With that completed, we are all done updating our application for replication. The only remaining method, `printStocks()`, is unmodified from when we originally introduced it. For details on that function, see <a href="simpleprogramlisting.md#printstocks_c" class="xref" title="Method: SimpleTxn.printStocks()"><span>Method: SimpleTxn.printStocks()</span></a> .

### Running It

To run our replicated application, we need to make sure each participating environment has its own unique home directory. We can do this by running each site on a separate networked machine, but that is not strictly necessary; multiple instances of this code can run on the same machine provided the environment home restriction is observed.

To run a process, make sure the environment home exists and then start the process using the `-h` option to specify that directory. You must also use the `-l` or `-L` option to identify the local host and port that this process will use to listen for replication messages (-L means that this is a group creator), and the `-r` option to identify the other processes in the replication group. Finally, use the `-p` option to specify a priority. The process that you designate to have the highest priority will become the master.

``` c
> mkdir env1
> java db.repquote_gsg.RepQuoteExampleGSG -h env1 -L localhost:8080 \
-p 10
No stock database yet available.
No stock database yet available.  
```

Now, start another process. This time, change the environment home to something else, use the `-l` flag to at least change the port number the process is listening on, and use the `-r` option to identify the host and port of the other replication process:

``` c
> mkdir env2
> java db.repquote_gsg.RepQuoteExampleGSG -h env2 -l localhost:8081 \
-r localhost:8080 -p 20
```

After a short pause, the second process should display the master prompt:

``` c
QUOTESERVER > 
```

And the first process should display the read-only prompt:

``` c
QUOTESERVER (read-only)> 
```

Now go to the master process and give it a couple of stocks and stock prices:

``` c
QUOTESERVER> FAKECO 9.87
QUOTESERVER> NOINC .23
QUOTESERVER> 
```

Then, go to the replica and hit **`return`** at the prompt to see the new values:

``` c
QUOTESERVER (read-only)> 
        Symbol  Price
        ======  =====
        FAKECO  9.87
        NOINC    .23 
QUOTESERVER (read-only)> 
```

Doing the same at the master results in the same thing:

``` c
QUOTESERVER> 
        Symbol  Price
        ======  =====
        FAKECO  9.87
        NOINC    .23 
QUOTESERVER> 
```

You can change a stock by simply entering the stock value and new price at the master's prompt:

``` c
QUOTESERVER> FAKECO 10.01 
QUOTESERVER> 
```

Then, go to either the master or the replica to see the updated database. On the master:

``` c
QUOTESERVER> 
        Symbol  Price
        ======  =====
        FAKECO  10.01
        NOINC    .23 
QUOTESERVER> 
```

And on the replica:

``` c
QUOTESERVER (read-only)> 
        Symbol  Price
        ======  =====
        FAKECO  10.01
        NOINC    .23 
QUOTESERVER (read-only)> 
```

Finally, to quit the applications, simply type `quit` at both prompts. On the replica:

``` c
QUOTESERVER (read-only)> quit
> 
```

And on the master as well:

``` c
QUOTESERVER> quit
> 
```
