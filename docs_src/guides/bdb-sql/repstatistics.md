---
title: "Displaying Replication Statistics"
api-name: "Displaying Replication Statistics"
source: docs/bdb-sql/repstatistics.html
---
## Displaying Replication Statistics

You can display a brief summary of replication statistics using `.stat :rep:`. This command displays the most basic information about replication status, as well as some information that is useful for troubleshooting.

``` c
dbsql> .stat :rep:
Replication summary statistics
Environment configured as a replication client
Startup complete
1/50232 Maximum permanent LSN
2       Number of environments in the replication group
0       Number of failed message sends
0       Number of messages ignored due to pending recovery
0       Number of log records currently queued 
```

In the above output:

- `Environment configured as a replication client`

  Identifies the current role of the site within the replication group. In this example, the current site is not the master site. <span class="emphasis">*Replication client*</span> is another term for <span class="emphasis">*replica*</span>.

- `Startup complete`

  Indicates that this replica site has completed its synchronization with the master site. Replica synchronization can take some time if there are many master transactions with which it needs to catch up.

- `Maximum permanent LSN`

  Identifies the most recent log record that is durably replicated on a master or acknowledged by a replica. You can compare a replica's maximum permanent LSN to the master's maximum permanent LSN to determine if the replica is caught up with the master.

- `Number of failed message sends`

  If this number is increasing, it could be an indication of network or communications problems between sites in the replication group.

- `Number of messages ignored due to pending recovery`

  If this number is increasing, this site is ignoring messages because it is starting up or recovering and may need some time to catch up with the rest of the replication group.

- `Number of log records currently queued`

  If this number is increasing, it means that connections to other sites may be unavailable or congested and that there may be delays in durably replicating master transactions.
