---
title: "Permanent Message Handling"
api-name: "Permanent Message Handling"
source: docs/gsg_db_rep/JAVA/fwrkpermmessage.html
---
## Permanent Message Handling

<span class="sect2"> [Identifying Permanent Message Policies](fwrkpermmessage.md#fmwrkpermpolicy) </span>

<span class="sect2"> [Setting the Permanent Message Timeout](fwrkpermmessage.md#fmwrkpermtimeout) </span>

<span class="sect2"> [Adding a Permanent Message Policy to RepQuoteExampleGSG](fwrkpermmessage.md#perm2fmwrkexample) </span>

As described in <a href="permmessages.md" class="xref" title="Permanent Message Handling">Permanent Message Handling</a>, messages are marked permanent if they contain database modifications that should be committed at the replica. DB's replication code decides if it must flush its transaction logs to disk depending on whether it receives sufficient permanent message acknowledgments from the participating replicas. More importantly, the thread performing the transaction commit blocks until it either receives enough acknowledgments, or the acknowledgment timeout expires.

The Replication Manager is fully capable of managing permanent messages for you if your application requires it (most do). Almost all of the details of this are handled by the Replication Manager for you. However, you do have to set some policies that tell the Replication Manager how to handle permanent messages.

There are two things that you have to do:

- Determine how many acknowledgments must be received by the master.

- Identify the amount of time that replicas have to send their acknowledgments.

### Identifying Permanent Message Policies

You identify permanent message policies using the `ReplicationManagerAckPolicy` class which you pass to the environment using the `EnvironmentConfig.setReplicationManagerAckPolicy` method. Note that you can set permanent message policies at any time during the life of the application.

The following permanent message policies are available when you use the Replication Manager:

### Note

The following list mentions <span class="emphasis">*electable peer*</span> several times. This is simply another environment that can be elected to be a master (that is, it has a priority greater than 0). Do not confuse this with the concept of a peer as used for client to client transfers. See <a href="c2ctransfer.md" class="xref" title="Client to Client Transfer">Client to Client Transfer</a> for more information on client to client transfers.

- `ReplicationManagerAckPolicy.NONE`

  No permanent message acknowledgments are required. If this policy is selected, permanent message handling is essentially "turned off." That is, the master will never wait for replica acknowledgments. In this case, transaction log data is either flushed or not strictly depending on the type of commit that is being performed (synchronous or asynchronous).

- `ReplicationManagerAckPolicy.ONE`

  At least one replica must acknowledge the permanent message within the timeout period.

- `ReplicationManagerAckPolicy.ONE_PEER`

  At least one electable peer must acknowledge the permanent message within the timeout period.

- `ReplicationManagerAckPolicy.ALL`

  All replicas must acknowledge the message within the timeout period. This policy should be selected only if your replication group has a small number of replicas, and those replicas are on extremely reliable networks and servers.

- `ReplicationManagerAckPolicy.ALL_AVAILABLE`

  All currently connected replication clients must acknowledge the message. This policy will invoke the `DB_EVENT_REP_PERM_FAILED` event if fewer than a quorum of clients acknowledged during that time.

- `ReplicationManagerAckPolicy.ALL_PEERS`

  All electable peers must acknowledge the message within the timeout period. This policy should be selected only if your replication group is small, and its various environments are on extremely reliable networks and servers.

- `ReplicationManagerAckPolicy.QUORUM`

  A quorum of electable peers must acknowledge the message within the timeout period. A quorum is reached when acknowledgments are received from the minimum number of environments needed to ensure that the record remains durable if an election is held. That is, the master wants to hear from enough electable replicas that they have committed the record so that if an election is held, the master knows the record will exist even if a new master is selected.

By default, a quorum of electable peers must must acknowledge a permanent message in order for it considered to have been successfully transmitted.

### Setting the Permanent Message Timeout

The permanent message timeout represents the maximum amount of time the committing thread will block waiting for message acknowledgments. If sufficient acknowledgments arrive before this timeout has expired, the thread continues operations as normal. However, if this timeout expires, the committing thread flushes its transaction log buffer before continuing with normal operations.

You set the timeout value using `Environment.setReplicationTimeout()`. You pass this method the `ReplicationTimeoutType.ACK_TIMEOUT` constant and a timeout value in microseconds.

For example:

``` c
  dbenv.setReplicationTimeout(ReplicationTimeoutType.ACK_TIMEOUT, 100); 
```

This timeout value can be set at anytime during the life of the application.

### Adding a Permanent Message Policy to RepQuoteExampleGSG

For illustration purposes, we will now update `RepQuoteExampleGSG` such that it requires only one acknowledgment from a replica on transactional commits. Also, we will give this acknowledgment a 500 microsecond timeout value. This means that our application's main thread will block for up to 500 microseconds waiting for an acknowledgment. If it does not receive at least one acknowledgment in that amount of time, DB will flush the transaction logs to disk before continuing on.

This is a very simple update. We can perform the entire thing in `RepQuoteExampleGSG.init()` immediately after we set the application's priority and before we open our environment handle.

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
          repConfig.getFirstOtherHost();
          host != null; host = repConfig.getNextOtherHost()){

            ReplicationManagerSiteConfig repmgrRemoteSiteConfig =
               new ReplicationManagerSiteConfig(host.host, host.port);
            repmgrRemoteSiteConfig.setBootstrapHelper(true);
            envConfig.addReplicationManagerSite(
                repmgrRemoteSiteConfig);
        }
        envConfig.setReplicationPriority(appConfig.priority);

        envConfig.setReplicationManagerAckPolicy(
                ReplicationManagerAckPolicy.ALL);
        envConfig.setReplicationTimeout(ReplicationTimeoutType.ACK_TIMEOUT,
                500); 

        envConfig.setCacheSize(RepConfig.CACHESIZE);
        envConfig.setTxnNoSync(true);
    ... 
```
