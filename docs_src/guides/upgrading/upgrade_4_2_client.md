---
title: "DB_CLIENT"
api-name: "DB_CLIENT"
source: docs/upgrading/upgrade_4_2_client.html
---
## DB_CLIENT

The flag to create a client to connect to a RPC server was renamed from DB_CLIENT to DB_RPCCLIENT, in order to avoid confusion between RPC clients and replication clients. Applications using the DB_CLIENT flag should change that use to DB_RPCCLIENT; no other change is required.
