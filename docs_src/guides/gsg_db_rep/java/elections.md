---
title: "Holding Elections"
api-name: "Holding Elections"
source: docs/gsg_db_rep/JAVA/elections.html
---
## Holding Elections

<span class="sect2"> [Influencing Elections](elections.md#influencingelections) </span>

<span class="sect2"> [Winning Elections](elections.md#winningelections) </span>

<span class="sect2"> [Switching Masters](elections.md#switchingmasters) </span>

Finding a master environment is one of the fundamental activities that every replication replica must perform. Upon startup, the underlying DB replication code will attempt to locate a master. If a master cannot be found, then the environment should initiate an election.

### Note

In some rare situations, it is desireable for the application to manually select its master. For these cases, elections can be turned off.

Manually selecting a master is an activity that should be performed infrequently, if ever. You turn elections off by using the `ReplicationConfig` and `ReplicationManagerStartPolicy` classes.

How elections are held depends upon the API that you use to implement replication. For example, if you are using the Replication Manager elections are held transparently without any input from your application's code. In this case, DB will determine which environment is the master and which are replicas.

### Influencing Elections

If you want to control the election process, you can declare a specific environment to be the master. Note that for the Replication Manager, it is only possible to do this at application startup. Should the master become unavailable during run-time for any reason, an election is held. The environment that receives the most number of votes, wins the election and becomes the master. A machine receives a vote because it has the most up-to-date log records.

Because ties are possible when elections are held, it is possible to influence which environment will win the election. How you do this depends on which API you are using. In particular, if you are writing a custom replication layer, then there are a great many ways to manually influence elections.

One such mechanism is priorities. When votes are cast during an election, the winner is determined first by the environment with the most up-to-date log records. But if this is a tie, the the environment's priority is considered. So given two environments with log records that are equally recent, votes are cast for the environment with the higher priority.

Therefore, if you have a machine that you prefer to become a master in the event of an election, assign it a high priority. Assuming that the election is held at a time when the preferred machine has up-to-date log records, that machine will win the election.

### Winning Elections

To win an election:

1.  There cannot currently be a master environment.

2.  The environment must have the most recent log records. Part of holding the election is determining which environments have the most recent log records. This process happens automatically; your code does not need to involve itself in this process.

3.  The environment must receive the most number of votes from the replication environments that are participating in the election.

If you are using the Replication Manager, then in the event of a tie vote the environment with the highest priority wins the election. If two or more environments receive the same number of votes and have the same priority, then the underlying replication code picks one of the environments to be the winner. Which winner will be picked by the replication code is unpredictable from the perspective of your application code.

### Switching Masters

To switch masters:

1.  Start up the environment that you want to be master as normal. At this time it is a replica. Make sure this environment has a higher priority than all the other environments.

2.  Allow the new environment to run for a time as a replica. This allows it to obtain the most recent copies of the log files.

3.  Shut down the current master. This should force an election. Because the new environment has the highest priority, it will win the election, provided it has had enough time to obtain all the log records.

4.  Optionally restart the old master environment. Because there is currently a master environment, an election will not be held and the old master will now run as a replica environment.
