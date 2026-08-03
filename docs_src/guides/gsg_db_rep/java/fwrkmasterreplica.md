---
title: "Chapter 4. Replica versus Master Processes"
api-name: "Chapter 4. Replica versus Master Processes"
source: docs/gsg_db_rep/JAVA/fwrkmasterreplica.html
---
## Chapter 4. Replica versus Master Processes

**Table of Contents**

<span class="sect1"> [Determining State](fwrkmasterreplica.md#determinestate) </span>

<span class="sect1"> [Processing Loop](processingloop.md) </span>

<span class="sect1"> [Example Processing Loop](exampledoloop.md) </span>

<span class="sect2"> [Running It](exampledoloop.md#runningit) </span>

Every environment participating in a replicated application must know whether it is a <span class="emphasis">*master*</span> or <span class="emphasis">*replica*</span>. The reason for this is because, simply, the master can modify the database while replicas cannot. As a result, not only will you open databases differently depended on whether the environment is running as a master, but the environment will frequently behave quite a bit differently depending on whether it thinks it is operating as the read/write interface for your database.

Moreover, an environment must also be capable of gracefully switching between master and replica states. This means that the environment must be able to detect when it has switched states.

Not surprisingly, a large part of your application's code will be tied up in knowing which state a given environment is in and then in the logic of how to behave depending on its state.

This chapter shows you how to determine your environment's state, and it then shows you some sample code on how an application might behave depending on whether it is a master or a replica in a replicated application.

## Determining State

In order to determine whether your code is running as a master or a replica, you must write your application as an implementation of `com.sleepycat.db.EventHandler`. This class gives you a series of methods within which you can detect and respond to various events that occur in your DB code. Some, but not all, of these methods have to do with elections:

Some of the more commonly handled events are described below. For a complete list of events, see the `com.sleepycat.db.EventHandler` javadoc page.

- `EventHandler.handlePanicEvent()`

  An error has occured in the Berkeley DB library requiring your application to shut down and then run recovery.

- `EventHandler.handleRepClientEvent()`

  The local environment is now a replica.

- `EventHandler.handleRepConnectBrokenEvent()`

  A previously established connection between two sites in the replication group has been broken.

- `EventHandler.handleRepConnectEstablishedEvent()`

  A connection has been established between two sites in the replication group.

- `EventHandler.handleRepConnectTryFailedEvent()`

  An attempt was made to establish a connection to a known remote site, but the connection attempt failed.

- `EventHandler.handleRepDupmasterEvent()`

  A duplicate master has been discovered in the replication group.

- `EventHandler.handleRepElectedEvent()`

  The local site has just won an election and is now the master. Your code should now reconfigure itself to operation as a master site.

- `EventHandler.handleRepElectionFailedEvent()`

  The local site's attempt to initiate or participate in a replication master election failed, due to the lack of timely message response from a sufficient number of remote sites.

- `EventHandler.handleRepJoinFailureEvent()`

  The local site could not synchronize with the master because an internal initialization was required, but internal initialization has been turned off

- `EventHandler.handleRepLocalSiteRemovedEvent()`

  The local site has been removed from the group.

- `EventHandler.handleRepNewMasterEvent()`

  An election was held and a new environment was made a master. However, the current environment <span class="emphasis">*is not*</span> the master. This event exists so that you can cause your code to take some unique action in the event that the replication groups switches masters.

- `EventHandler.handleRepMasterEvent()`

  The local environment is now a master.

- `EventHandler.handleRepMasterFailureEvent()`

  The connection to the remote master replication site has failed.

- `EventHandler.handleRepPermFailedEvent()`

  The Replication Manager did not receive enough acknowledgements to ensure the transaction's durability within the replicationg group. The Replication Manager has therefore flushed the transaction to the master's local disk for storage.

  How the Replication Manager knows whether the acknowledgements it has received is determined by the ack policy you have set for your applicaton. See <a href="fwrkpermmessage.md#fmwrkpermpolicy" class="xref" title="Identifying Permanent Message Policies">Identifying Permanent Message Policies</a> for more information.

- `EventHandler.handleRepSiteAddedEvent()`

  A new site has joined the replication group.

- `EventHandler.handleRepSiteRemovedEvent()`

  An existing site has been removed from the replication group.

- `EventHandler.handleRepStartupDoneEvent()`

  The replica has completed startup synchronization and is now processing log records received from the master.

- `EventHandler.handleWriteFailedEvent()`

  A Berkeley DB write to stable storage failed.

Note that these events are raised whenever the state is established. That is, when the current environment becomes a replica, and that includes at application startup, the event is raised. Also, when an election is held and a replica is elected to be a master, then the event occurs.

The `EventHandler` implementation is fairly simple. First you detect the event, and then you record the state change in some data member maintained in a location that is convenient to you.

For example:

``` c
package db.repquote;

// We make our main class an EventHandler implementation
...
import com.sleepycat.db.EventHandler;
...

public class MyReplicationClass implements EventHandler
{

...

// Somewhere we provide a data member that is used to track
// whether we are a master server. This could be in our main
// class, or it could be part of a supporting class.
private boolean isMaster;

...

isMaster = false;

...

// In the code where we open our environment and start replication,
// we must identify the class that is the event handler. In this
// example, we are performing this from within the class that 
// implements com.sleepycat.db.EventHandler so we identify
// "this" class as the event handler
envConfig.setEventHandler(this); 
```

That done, we still need to implement the methods required for handling replication events. For a simple application like this one, these implementations can be trivial.

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
        // Ignored for now
    }

    public void handleWriteFailedEvent(int errorCode)
    {
        System.err.println("Write to stable storage failed!" +
            "Operating system error code:" + errorCode);
        System.err.println("Continuing....");
    }

    public void handleRepStartupDoneEvent()
    {
        System.out.println("Replication startup is completed.");
    }

    public void handleRepPermFailedEvent()
    {
        System.out.println("This application failed to receive enough" +
            "acks for a permanent message. The transaction is flushed" + 
            "to disk on this master host.");
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

Of course, this only gives us the current state of the environment. We still need the code that determines what to do when the environment changes state and how to behave depending on the state (described in the next section).
