---
title: "Adding the Replication Manager to SimpleTxn"
api-name: "Adding the Replication Manager to SimpleTxn"
source: docs/gsg_db_rep/JAVA/repmgr_init_example_c.html
---
## Adding the Replication Manager to SimpleTxn

We now use the methods described above to add partial support to the SimpleTxn example that we presented in <a href="txnapp.md" class="xref" title="Chapter 2. Transactional Application">Transactional Application</a>. That is, in this section we will:

- Enhance our command line options to accept information of interest to a replicated application.

- Configure our environment handle to use replication and the Replication Manager.

- Minimally configure the Replication Manager.

- Start replication.

Note that when we are done with this section, we will be only partially ready to run the application. Some critical pieces will be missing; specifically, we will not yet be handling the differences between a master and a replica. (We do that in the next chapter).

Also, note that in the following code fragments, additions and changes to the code are marked in **`bold`**.

To begin, we make some significant changes to our `RepConfig` class because we will be using it to maintain a lot more information that we needed for our simple transactional example.

We begin by importing a few new classes. `java.util.Vector` is used to organize a list of "other host" definitions (that is, the host and port information for the other replication participants known to this application). We also need a couple of classes used to manage individual host and port information, as well as replication sites and startup policy information.

``` c
package db.repquote_gsg;

import java.util.Vector;

import com.sleepycat.db.ReplicationHostAddress;
import com.sleepycat.db.ReplicationManagerSiteConfig;
import com.sleepycat.db.ReplicationManagerStartPolicy;

public class RepConfig
{ 
```

Next we add considerably to the constants and data members used by this class. All of this is used to manage information necessary for replication purposes. We also at this point change the program's name, since we will be doing that to the main class in our application a little later in this description.

``` c
    // Constant values used in the RepQuote application.
    public static final String progname = "RepQuoteExampleGSG";
    public static final int CACHESIZE = 10 * 1024 * 1024;
    public static final int SLEEPTIME = 5000;

    // member variables containing configuration information
    // String specifying the home directory for rep files.
    public String home;
    // Stores an optional set of "other" hosts.
    public Vector<ReplicationHostAddress> otherHosts;
    // Priority within the replication group.
    public int priority; 
    public ReplicationManagerStartPolicy startPolicy;
    // The host address to listen to.
    public ReplicationHostAddress thisHost; 

    // member variables used internally.
    private int currOtherHost;
    private boolean gotListenAddress;
```

Now we update our class constructor to initialize all of these new variables:

``` c
    public RepConfig()
    {
        startPolicy = ReplicationManagerStartPolicy.REP_ELECTION;
        home = "TESTDIR";
        gotListenAddress = false;
        priority = 100;
        currOtherHost = 0;
        thisHost = new ReplicationManagerSiteConfig();
        otherHosts = new Vector();
    } 
```

Finally, we finish updating this class by providing a series of new getter and setter methods. These are used primarily for setting a retrieving host information of interest to our replicated application:

``` c
    public java.io.File getHome()
    {
        return new java.io.File(home);
    }

    public void setThisHost(String host, int port, boolean creator)
    {
        gotListenAddress = true;
        thisHost.setHost(host);
        thisPort.setPort(port);
        thisHost.setGroupCreator(creator);
    }

    public ReplicationManagerSiteConfig getThisHost()
    {
        if (!gotListenAddress) {
            System.err.println("Warning: no host specified.");
            System.err.println("Returning default.");
        }
        return thisHost;
    }

    public ReplicationHostAddress getThisHostAddress()
    {
        if (!gotListenAddress) {
            System.err.println("Warning: no host specified.");
            System.err.println("Returning default.");
        }
        return thisHost.getAddress();
    } 

    public boolean gotListenAddress() {
        return gotListenAddress;
    }

    public void addOtherHost(String host, int port)
    {
        ReplicationHostAddress newInfo = 
            new ReplicationHostAddress(host, port);
        otherHosts.add(newInfo);
    }

    public ReplicationHostAddress getFirstOtherHost()
    {
        currOtherHost = 0;
        if (otherHosts.size() == 0)
            return null;
        return (ReplicationHostAddress)otherHosts.get(currOtherHost);
    }

    public ReplicationHostAddress getNextOtherHost()
    {
        currOtherHost++;
        if (currOtherHost >= otherHosts.size())
            return null;
        return (ReplicationHostAddress)otherHosts.get(currOtherHost);
    }

    public ReplicationHostAddress getOtherHost(int i)
    {
        if (i >= otherHosts.size())
            return null;
        return (ReplicationHostAddress)otherHosts.get(i);
    }
} 
```

Having completed our update to the `RepConfig` class, we can now start making changes to the main portion of our program. We begin by changing the program's name. (This, of course, means that we copy our `SimpleTxn` code to a file named `RepQuoteExampleGSG.java`.)

``` c
package db.repquote_gsg;
                            
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
import com.sleepycat.db.LockMode;
import com.sleepycat.db.OperationStatus;
import com.sleepycat.db.ReplicationHostAddress;
import com.sleepycat.db.ReplicationManagerSiteConfig;

import db.repquote.RepConfig;

public class RepQuoteExampleGSG
{
    private RepConfig repConfig;
    private Environment dbenv; 
```

Next we update our usage function. The application will continue to accept the `-h` parameter so that we can identify the environment home directory used by this application. However, we also add the:

- `-l` parameter which allows us to identify the host and port used by this application to listen for replication messages. This parameter is required unless the -L parameter is specified.

- `-L` parameter, which allows us to identify the local site as the group creator.

- `-r` parameter which allows us to specify other replicas.

- `-p` option, which is used to identify this replica's priority (recall that the priority is used as a tie breaker for elections)

``` c
    public RepQuoteExampleGSG()
        throws DatabaseException
    {
        repConfig = null;
        dbenv = null;
    }

    public static void usage()
    {
        System.err.println("usage: " + repConfig.progname);
        System.err.println("-h home[-r host:port][-l|-L host:port]" +
            "[-r host:port][-p priority]");

        System.err.println("\t -h home directory (required)\n" +
            "\t -l host:port (required, unless -L is specified; " +
             "l stands for local)\n" +
             "\t -L host:port (optional;L mean group creator)\n" +
             "\t -r host:port (optional; r stands for remote; any " +
             "number of these may be specified)\n" +
             "\t -p priority (optional: defaults to 100)\n");

        System.exit(1);
    } 
```

Now we can begin working on our `main()` function. We begin by adding a couple of variables that we will use to collect TCP/IP host and port information.

``` c
    public static void main(String[] argv)
        throws Exception
    {
        RepConfig config = new RepConfig();
        String tmpHost;
        int tmpPort = 0; 
```

Now we collect our command line arguments. As we do so, we will configure host and port information as required, and we will configure the application's election priority if necessary.

``` c
        // Extract the command line parameters
        for (int i = 0; i < argv.length; i++)
        {
            if (argv[i].compareTo("-h") == 0) {
                // home is a string arg.
                i++;
                config.home = argv[i];
            } else if (argv[i].compareTo("-l") == 0) ||
              argv[i].compareTo("-L") == 0) {
                if (i == argv.length - 1)
                    usage();
                if (argv[i].compareTo("-L") == 0)
                    isCreator = true;
                // "local" should be host:port.
                i++;
                String[] words = argv[i].split(":");
                if (words.length != 2) {
                   System.err.println(
                     "Invalid host specification host:port needed.");
                    usage();
                }
                try {
                    tmpPort = Integer.parseInt(words[1]);
                } catch (NumberFormatException nfe) {
                    System.err.println("Invalid host specification, " +
                        "could not parse port number.");
                    usage();
                }
                config.setThisHost(words[0], tmpPort, isCreator);
            } else if (argv[i].compareTo("-p") == 0) {
                i++;
                config.priority = Integer.parseInt(argv[i]);
            } else if (argv[i].compareTo("-r") == 0) {
                i++;
                String[] words = argv[i].split(":");
                if (words.length != 2) {
                   System.err.println(
                     "Invalid host specification host:port needed.");
                   usage();
                }
                try {
                    tmpPort = Integer.parseInt(words[1]);
                } catch (NumberFormatException nfe) {
                    System.err.println("Invalid host specification, " +
                        "could not parse port number.");
                    usage();
                }
                config.addOtherHost(words[0], tmpPort);
            } else {
                System.err.println("Unrecognized option: " + argv[i]);
                usage();
            }
        } 

        // Error check command line.
        if ((!config.gotListenAddress()) || config.home.length() == 0)
            usage(); 
```

Having done that, the remainder of our `main()` function is left unchanged, with the exception of a few name changes required by the new class name:

``` c
        RepQuoteExampleGSG runner = null;
        try {
            runner = new RepQuoteExampleGSG();
            runner.init(config);

            runner.doloop();
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

Now we need to update our `RepQuoteExampleGSG.init()` method. Our updates are at first related to configuring replication. First, we need to update the method so that we can identify the local site to the environment handle (that is, the site identified by the `-l` command line option):

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
```

And we also add code to allow us to identify "other" sites to the environment handle (that is, the sites that we identify using the `-o` command line option). To do this, we iterate over each of the "other" sites provided to us using the `-o` command line option, and we add each one individually in turn:

We also add code here to set the environment's priority.

``` c
        
        for (ReplicationHostAddress host = 
            repConfig.getFirstOtherHost(); host != null; 
            host = repConfig.getNextOtherHost()) {

            ReplicationManagerSiteConfig repmgrRemoteSiteConfig =
                new ReplicationManagerSiteConfig(host.host, host.port);
            repmgrRemoteSiteConfig.setBootstrapHelper(true);
            envConfig.addReplicationManagerSite(
                repmgrRemoteSiteConfig);
        }

        envConfig.addReplicationPriority(repConfig.priority);
         
```

We can now open our environment. Note that the options we use to open the environment are slightly different for a replicated application than they are for a non-replicated application. Namely, replication requires the `EnvironmentConfig.setInitializeReplication()` option.

Also, because we are using the Replication Manager, we must prepare our environment for threaded usage. For this reason, we also need the `DB_THREAD` flag.

``` c
        envConfig.setCacheSize(RepConfig.CACHESIZE);
        envConfig.setTxnNoSync(true);

        envConfig.setAllowCreate(true);
        envConfig.setRunRecovery(true);
        envConfig.setInitializeReplication(true);
        envConfig.setInitializeLocking(true);
        envConfig.setInitializeLogging(true);
        envConfig.setInitializeCache(true);
        envConfig.setTransactional(true);
        try {
            dbenv = new Environment(appConfig.getHome(), envConfig);
        } catch(FileNotFoundException e) {
            System.err.println("FileNotFound exception: " + e.toString());
            System.err.println(
                "Ensure that the environment directory is pre-created.");
            ret = 1;
        } 
```

Finally, we start replication before we exit this method. Immediately after exiting this method, our application will go into the `RepQuoteExampleGSG.doloop()` method, which is where the bulk of our application's work is performed. We update that method in the next chapter.

``` c
        // start Replication Manager
        dbenv.replicationManagerStart(3, appConfig.startPolicy);
        return ret;
    } 
```

This completes our replication updates for the moment. We are not as yet ready to actually run this program; there remains a few critical pieces left to add to it. However, the work that we performed in this section represents a solid foundation for the remainder of our replication work.
