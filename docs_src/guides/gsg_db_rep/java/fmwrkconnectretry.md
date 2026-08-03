---
title: "Managing Connection Retries"
api-name: "Managing Connection Retries"
source: docs/gsg_db_rep/JAVA/fmwrkconnectretry.html
---
## Managing Connection Retries

In the event that a communication failure occurs between two environments in a replication group, the Replication Manager will wait a set amount of time before attempting to re-establish the connection. You can configure this wait value using `Environment.setReplicationTimeout()`. You pass this method the `ReplicationTimeoutType.CONNECTION_RETRY` constant and a retry value in microseconds.
