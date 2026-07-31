---
title: "Managing Heartbeats"
api-name: "Managing Heartbeats"
source: docs/gsg_db_rep/C/heartbeats.html
---
## Managing Heartbeats

If your replicated application experiences few updates, it is possible for the replication group to lose a master without noticing it. This is because normally a replicated application only knows that a master has gone missing when update activity causes messages to be passed between the master and replicas.

To guard against this, you can configure a heartbeat. The heartbeat must be configured for both the master and each of the replicas.

On the master, you configure the application to send a heartbeat on a defined interval when it is otherwise idle. Do this by using the `DB_REP_HEARTBEAT_SEND` value to the `which` parameter of the `DB_ENV->rep_set_timeout()` method. You must also provide the method a value representing the period between heartbeats in microseconds. Note that the heartbeat is sent only if the system is idle.

On the replica, you configure the application to listen for a heartbeat. The time that you configure here is the amount of time the replica will wait for some message from the master (either the heartbeat or some other message) before concluding that the connection is lost. You do this using the `DB_REP_HEARTBEAT_MONITOR` value to the `which` parameter of the `DB_ENV->rep_set_timeout()` method and a timeout value in microseconds.

For best results, configure the heartbeat monitor for a longer time interval than the heartbeat send interval.
