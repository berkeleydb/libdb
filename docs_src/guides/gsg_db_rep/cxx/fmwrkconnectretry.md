---
title: "Managing Connection Retries"
api-name: "Managing Connection Retries"
source: docs/gsg_db_rep/CXX/fmwrkconnectretry.html
---
## Managing Connection Retries

In the event that a communication failure occurs between two environments in a replication group, the Replication Manager will wait a set amount of time before attempting to re-establish the connection. You can configure this wait value using `DbEnv::rep_set_timeout()`. To do so, specify the `DB_REP_CONNECTION_RETRY` value to the `which` parameter and then a retry value in microseconds to the `timeout` parameter.
