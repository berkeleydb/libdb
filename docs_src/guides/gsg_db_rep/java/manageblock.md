---
title: "Managing Blocking Operations"
api-name: "Managing Blocking Operations"
source: docs/gsg_db_rep/JAVA/manageblock.html
---
## Managing Blocking Operations

When a replica is in the process of synchronizing with its master, DB operations are blocked at some points during this process until the synchronization is completed. For replicas with a heavy read load, these blocked operations may represent an unacceptable loss in throughput.

You can configure DB so that it will not block when synchronization is in process. Instead, the DB operation will fail, immediately throwing a `com.sleepycat.db.ReplicationLockoutException` exception. When this happens, it is up to your application to determine what action to take (that is, logging the event, making an appropriate user response, retrying the operation, and so forth).

To turn off blocking on synchronization, specify `ReplicationConfig.NOWAIT` and `true` to `Environment.setReplicationConfig()`. To turn off this feature, specify `false` for the `ReplicationConfig.NOWAIT` field.
