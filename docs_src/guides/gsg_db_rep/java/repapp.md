---
title: "Chapter 3. The DB Replication Manager"
api-name: "Chapter 3. The DB Replication Manager"
source: docs/gsg_db_rep/JAVA/repapp.html
---
## Chapter 3. The DB Replication Manager

**Table of Contents**

<span class="sect1"> [Starting and Stopping Replication](repapp.md#rep_init_code) </span>

<span class="sect2"> [Managing Election Policies](repapp.md#election_flags) </span>

<span class="sect2"> [Selecting the Number of Threads](repapp.md#thread_count) </span>

<span class="sect1"> [Adding the Replication Manager to SimpleTxn](repmgr_init_example_c.md) </span>

<span class="sect1"> [Permanent Message Handling](fwrkpermmessage.md) </span>

<span class="sect2"> [Identifying Permanent Message Policies](fwrkpermmessage.md#fmwrkpermpolicy) </span>

<span class="sect2"> [Setting the Permanent Message Timeout](fwrkpermmessage.md#fmwrkpermtimeout) </span>

<span class="sect2"> [Adding a Permanent Message Policy to RepQuoteExampleGSG](fwrkpermmessage.md#perm2fmwrkexample) </span>

<span class="sect1"> [Managing Election Times](electiontimes.md) </span>

<span class="sect2"> [Managing Election Timeouts](electiontimes.md#electiontimeout) </span>

<span class="sect2"> [Managing Election Retry Times](electiontimes.md#electretrytime) </span>

<span class="sect1"> [Managing Connection Retries](fmwrkconnectretry.md) </span>

<span class="sect1"> [Managing Heartbeats](heartbeats.md) </span>

The easiest way to add replication to your transactional application is to use the Replication Manager. The Replication Manager provides a comprehensive communications layer that enables replication. For a brief listing of the Replication Manager's feature set, see <a href="apioverview.md#repframeworkoverview" class="xref" title="Replication Manager Overview">Replication Manager Overview</a>.

To use the Replication Manager, you make use of special methods off the `Environment` and `EnvironmentConfig` classes, as well as the `ReplicationManagerSiteConfig` class. You also use a series of related classes to perform your implementation. For example, in order to detect whether your code is running as a master or a replica, you must implement `com.sleepycat.db.EventHandler`. (see <a href="fwrkmasterreplica.md#determinestate" class="xref" title="Determining State">Determining State</a>). That is:

1.  Create an environment handle as normal.

2.  Configure your environment handle as needed (e.g. set the error file and error prefix values, if desired).

3.  Use the Replication Manager replication classes and methods to configure the Replication Manager. Using these classes and methods causes DB to know that you are using the Replication Manager.

    Configuring the Replication Manager entails setting the replication environment's priority, setting the TCP/IP address that this replication environment will use for incoming replication messages, identifying TCP/IP addresses of other replication environments, setting the number of replication environments in the replication group, and so forth. These actions are discussed throughout the remainder of this chapter.

4.  Open your environment handle. When you do this, be sure to specify `EnvironmentConfig.setInitializeReplication()` when you configure your environment handle. This is in addition to the configuration that you would normally use for a transactional application. This causes replication to be initialized for the application.

5.  Start replication by calling `Environment.replicationManagerStart()`.

6.  Open your databases as needed. Masters must open their databases for read and write activity. Replicas can open their databases for read-only activity, but doing so means they must re-open the databases if the replica ever becomes a master. Either way, replicas should never attempt to write to the database(s) directly.

### Note

The Replication Manager allows you to only use one environment handle per process.

When you are ready to shut down your application:

1.  Close your databases

2.  Close your environment. This causes replication to stop as well.

### Note

Before you can use the Replication Manager, you may have to enable it in your DB library. This is <span class="emphasis">*not*</span> a requirement for Microsoft Windows systems, or Unix systems that use pthread mutexes by default. Other systems, notably BSD and BSD-derived systems (such as Mac OS X), must enable the Replication Manager when you configure the DB build.

You do this by <span class="emphasis">*not*</span> disabling replication and by configuring the library with POSIX threads support. In other words, replication must be turned on in the build (it is by default), and POSIX thread support must be enabled if it is not already by default. To do this, use the `--enable-pthread_api` switch on the configure script.

For example:

``` c
../dist/configure --enable-pthread-api
```

## Starting and Stopping Replication

<span class="sect2"> [Managing Election Policies](repapp.md#election_flags) </span>

<span class="sect2"> [Selecting the Number of Threads](repapp.md#thread_count) </span>

As described above, you introduce replication to an application by starting with a transactional application, performing some basic replication configuration, and then starting replication using `Environment.replicationManagerStart()`.

You stop replication by closing your environment cleanly in the same way you would for any DB application.

For example, the following code fragment initializes, then stops and starts replication. Note that other replication activities are omitted for brevity.

### Note

Note that the following code fragment would be part of a larger class that must implement `com.sleepycat.db.EventHandler`. This class is used to track state changes between master and replica. We put off that implementation for the moment, but the point remains that the following code fragment would be contained in a method or two that you would include in your `com.sleepycat.db.EventHandler` implementation.

``` c
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;
import com.sleepycat.db.ReplicationHostAddress;
import com.sleepycat.db.ReplicationManagerStartPolicy;
import com.sleepycat.db.ReplicationManagerSiteConfig;

...
    String progname = "example_replication";
    String envHome = "TESTDIR";
    int cachesize = 10 * 1024 * 1024;

    Environment dbenv;
    String listenHost = "mymachine.sleepycat.com";
    String otherHost = "anothermachine.sleepycat.com";
    int listenPort = 8080;
    int otherPort = 8081;

try {
    // Configure the environment handle
    EnvironmentConfig envConfig = new EnvironmentConfig();
    envConfig.setErrorStream(System.err);
    envConfig.setErrorPrefix(progname);
    envConfig.setCacheSize(cachesize);
    envConfig.setTxnNoSync(true);

    // Identify the local replication site. This is the local hostname
    // and  port that this replication participant will use to receive
    // incoming replication messages. Note that this can be
    // performed only once for the application. It is required.
    ReplicationManagerSiteConfig thisHostConfig = 
        new ReplicationManagerSiteConfig(listenHost, listenPort);
    thisHostConfig.setGroupCreator(creator);
    thisHostConfig.setLocalSite(true)
    envConfig.addReplicationManagerSite(thisHostConfig);

    // Set this application's priority. This is used for elections.
    // 
    // Set this number to a positive integer, or 0 if you do not want
    // this site to be able to become a master.
    envConfig.setReplicationPriority(100);

    // Add a site to the list of replication environments known to 
    // this application.
    ReplicationManagerSiteConfig remoteSiteConfig =
        new ReplicationManagerSiteConfig(otherHost, otherPort);
    remoteSiteConfig.setBootstrapHelper(true);
    envConfig.addReplicationManagerSite(remoteSiteConfig);

    // Configure the environment's subsystems. Note that we initialize
    // replication. This is required.
    envConfig.setAllowCreate(true);
    envConfig.setRunRecovery(true);
    envConfig.setThreaded(true);
    envConfig.setInitializeReplication(true);
    envConfig.setInitializeLocking(true);
    envConfig.setInitializeLogging(true);
    envConfig.setInitializeCache(true);
    envConfig.setTransactional(true);

    // Missing from this is where we set the event handle and the
    // acknowledgement policy. We discuss these things later in this 
    // book. 

    // Open our environment handle.
    try {
        dbenv = new Environment(envHome, envConfig);
    } catch(FileNotFoundException e) {
        System.err.println("FileNotFound exception: " + e.toString());
        System.err.println(
            "Ensure that the environment directory is pre-created.");
    }

    // Start the replication manager such that it has three threads.
    dbenv.replicationManagerStart(3,
        ReplicationManagerStartPolicy.REP_ELECTION);

    //////////////////////////////////////////////////
    // All other application code goes here, including
    // database opens.
    //////////////////////////////////////////////////

} catch (DatabaseException dbe) {
    // Error handling goes here
}

// Close out your application here.
try {
    // Make sure all your databases are closed.

    // Closing your environment stops replication.
   dbenv.close();
} catch (DatabaseException dbe) {
    // Error handling here.
}

// All done.  
```

### Managing Election Policies

Before continuing, it is worth taking a look at the startup election options that you can set for replication. You set these using the `ReplicationManagerStartPolicy` class that you pass to the `Environment.replicationManagerStart()` method.

In the previous example, we specified `ReplicationManagerStartPolicy.REP_ELECTION` when we started replication. This causes the application to try to find a master upon startup. If it cannot, it calls for an election. In the event an election is held, the environment receiving the most number of votes will become the master.

There's some important points to make here:

- This option only requires that other environments in the replication group participate in the vote. There is no requirement that <span class="emphasis">*all*</span> such environments participate. In other words, if an environment starts up, it can call for an election, and select a master, even if all other environment have not yet joined the replication group.

- It only requires a simple majority of participating environments to elect a master. This is always true of elections held using the Replication Manager.

- As always, the environment participating in the election with the most up-to-date log files is selected as master. If an environment with more recent log files has not yet joined the replication group, it may not become the master.

Any one of these points may be enough to cause a less-than-optimum environment to be selected as master. Therefore, to give you a better degree of control over which environment becomes a master at application startup, the Replication Manager offers the following start-up options:

<table data-border="1" width="80%">
<thead>
<tr>
<th>Option</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr>
<td><code class="literal">ReplicationManagerStartPolicy. REP_MASTER</code></td>
<td><p>The application starts up and declares the environment to be a master without calling for an election. It is an error for more than one environment to start up using this flag, or for an environment to use this flag when a master already exists.</p>
<p>Note that no replication group should <span class="emphasis"><em>ever</em></span> operate with more than one master.</p>
<p>In the event that a environment attempts to become a master when a master already exists, the replication code will resolve the problem by holding an election. Note, however, that there is always a possibility of data loss in the face of duplicate masters, because once a master is selected, the environment that loses the election will have to roll back any transactions committed until it is in sync with the "real" master.</p></td>
</tr>
<tr>
<td><code class="literal">ReplicationManagerStartPolicy. REP_CLIENT</code></td>
<td><p>The application starts up and declares the environment to be a replica without calling for an election. Note that the environment can still become a master if a subsequent application starts up, calls for an election, and this environment is elected master.</p></td>
</tr>
<tr>
<td><code class="literal">ReplicationManagerStartPolicy. REP_ELECTION</code></td>
<td><p>As described above, the application starts up, looks for a master, and if one is not found calls for an election.</p></td>
</tr>
</tbody>
</table>

### Selecting the Number of Threads

Under the hood, the Replication Manager is threaded and you can control the number of threads used to process messages received from other replicas. The threads that the Replication Manager uses are:

- Incoming message thread. This thread receives messages from the site's socket and passes those messages to message processing threads (see below) for handling.

- Outgoing message thread. Outgoing messages are sent from whatever thread performed a write to the database(s). That is, the thread that called, for example, `Database.put()` is the thread that writes replication messages about that fact to the socket.

  Note that if this write activity would cause the thread to be blocked due to some condition on the socket, the Replication Manager will hand the outgoing message to the incoming message thread, and it will then write the message to the socket. This prevents your database write threads from blocking due to abnormal network I/O conditions.

- Message processing threads are responsible for parsing and then responding to incoming replication messages. Typically, a response will include write activity to your database(s), so these threads can be busy performing disk I/O.

Of these threads, the only ones that you have any configuration control over are the message processing threads. In this case, you can determine how many of these threads you want to run.

It is always a bit of an art to decide on a thread count, but the short answer is you probably do not need more than three threads here, and it is likely that one will suffice. That said, the best thing to do is set your thread count to a fairly low number and then increase it if it appears that your application will benefit from the additional threads.
