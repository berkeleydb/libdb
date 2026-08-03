---
title: "Bulk Transfers"
api-name: "Bulk Transfers"
source: docs/gsg_db_rep/JAVA/bulk.html
---
## Bulk Transfers

By default, messages are sent from the master to replicas as they are generated. This can degrade replication performance because the various participating environments must handle a fair amount of network I/O activity.

You can alleviate this problem by configuring your master environment for bulk transfers. Bulk transfers simply cause replication messages to accumulate in a buffer until a triggering event occurs. When this event occurs, the entire contents of the buffer is sent to the replica, thereby eliminating excessive network I/O.

Note that if you are using replica to replica transfers, then you might want any replica that can service replication requests to also be configured for bulk transfers.

The events that result in a bulk transfer of replication messages to a replica will differ depending on if the transmitting environment is a master or a replica.

If the servicing environment is a master environment, then bulk transfer occurs when:

1.  Bulk transfers are configured for the master environment, and

2.  the message buffer is full or

3.  a permanent record (for example, a transaction commit or a checkpoint record) is placed in the buffer for the replica.

If the servicing environment is a replica environment (that is, replica to replica transfers are in use), then a bulk transfer occurs when:

1.  Bulk transfers are configured for the transmitting replica, and

2.  the message buffer is full or

3.  the replica servicing the request is able to completely satisfy the request with the contents of the message buffer.

To configure bulk transfers, specify `ReplicationConfig.BULK` and `true` to `Environment.setReplicationConfig()`. To turn off this feature, specify `false` for the `ReplicationConfig.BULK` field.
