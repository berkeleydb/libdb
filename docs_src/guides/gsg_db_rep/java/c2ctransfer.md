---
title: "Client to Client Transfer"
api-name: "Client to Client Transfer"
source: docs/gsg_db_rep/JAVA/c2ctransfer.html
---
## Client to Client Transfer

<span class="sect2"> [Identifying Peers](c2ctransfer.md#fmwrkpeerserver) </span>

It is possible to use a replica instead of a master to synchronize another replica. This serves to take the request load off a master that might otherwise occur if multiple replicas attempted to synchronize with the master at the same time.

For best results, use this feature combined with the delayed synchronization feature (see <a href="addfeatures.md#delayedsync" class="xref" title="Delayed Synchronization">Delayed Synchronization</a>).

For example, suppose your replication group consists of four environments. Upon application startup, all three replicas will immediately attempt to synchronize with the master. But at the same time, the master itself might be busy with a heavy database write load.

To solve this problem, delay synchronization for two of the three replicas. Allow the third replica to synchronize as normal with the master. Then, start synchronization for each of the delayed replicas (since this is a manual process, you can do them one at a time if that best suits your application). Assuming you have configured replica to replica synchronization correctly, the delayed replicas will synchronize using the up-to-date replica, rather than using the master.

When you are using the Replication Manager, you configure replica to replica synchronization by declaring an environment to be a peer of another environment. If an environment is a peer, then it can be used for synchronization purposes.

### Identifying Peers

You can designate one replica to be a peer of another for replica to replica synchronization. You might want to do this if you have machines that you know are on fast, reliable network connections and so you are willing to accept the overhead of waiting for acknowledgments from those specific machines.

Note that peers are not required to be a bi-directional. That is, just because machine A declares machine B to be a peer, that does not mean machine B must also declare machine A to be a peer.

You declare a peer for the current environment when you add that environment to the list of known sites. You do this by constructing a `ReplicationHostAddress` object that specifies `true` for the `isPeer` parameter, and then providing that object to `EnvironmentConfig.replicationManagerAddRemoteSite()` when you add the remote site to the local replication site.
