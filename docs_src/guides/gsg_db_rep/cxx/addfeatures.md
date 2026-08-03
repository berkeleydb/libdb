---
title: "Chapter 5. Additional Features"
api-name: "Chapter 5. Additional Features"
source: docs/gsg_db_rep/CXX/addfeatures.html
---
## Chapter 5. Additional Features

**Table of Contents**

<span class="sect1"> [Delayed Synchronization](addfeatures.md#delayedsync) </span>

<span class="sect1"> [Managing Blocking Operations](manageblock.md) </span>

<span class="sect1"> [Stop Auto-Initialization](autoinit.md) </span>

<span class="sect1"> [Read-Your-Writes Consistency](rywc.md) </span>

<span class="sect1"> [Client to Client Transfer](c2ctransfer.md) </span>

<span class="sect2"> [Identifying Peers](c2ctransfer.md#fmwrkpeerserver) </span>

<span class="sect1"> [Bulk Transfers](bulk.md) </span>

Beyond the basic functionality that we have discussed so far in this book, there are several replication features that you should understand. These are all optional to use, but provide useful functionality under the right circumstances.

These additional features are:

1.  <a href="addfeatures.md#delayedsync" class="xref" title="Delayed Synchronization">Delayed Synchronization</a>

2.  <a href="manageblock.md" class="xref" title="Managing Blocking Operations">Managing Blocking Operations</a>

3.  <a href="autoinit.md" class="xref" title="Stop Auto-Initialization">Stop Auto-Initialization</a>

4.  <a href="c2ctransfer.md" class="xref" title="Client to Client Transfer">Client to Client Transfer</a>

5.  <a href="bulk.md" class="xref" title="Bulk Transfers">Bulk Transfers</a>

## Delayed Synchronization

When a replication group has a new master, all replicas must synchronize with that master. This means they must ensure that the contents of their local database(s) are identical to that contained by the new master.

This synchronization process can result in quite a lot of network activity. It can also put a large strain on the master server, especially if is part of a large replication group or if there is somehow a large difference between the master's database(s) and the contents of its replicas.

It is therefore possible to delay synchronization for any replica that discovers it has a new master. You would do this so as to give the master time to synchronize other replicas before proceeding with the delayed replicas.

To delay synchronization of a replica environment, you specify `DB_REP_CONF_DELAYCLIENT` to `DbEnv::rep_set_config()` and then specify `1` to the `onoff` parameter. (Specify `0` to turn the feature off.)

If you use delayed synchronization, then you must manually synchronize the replica at some future time. Until you do this, the replica is out of sync with the master, and it will ignore all database changes forwarded to it from the master.

You synchronize a delayed replica by calling `DbEnv::rep_sync()` on the replica that has been delayed.
