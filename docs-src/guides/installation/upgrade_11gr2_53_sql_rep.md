---
title: "Replication for Existing Databases in the SQL API"
api-name: "Replication for Existing Databases in the SQL API"
source: docs/installation/upgrade_11gr2_53_sql_rep.html
---
## Replication for Existing Databases in the SQL API

<span class="sect2"> [PRAGMAs With Permanent Effects](upgrade_11gr2_53_sql_rep.md#idp837896) </span>

<span class="sect2"> [PRAGMAs That Can Now Operate on Existing Databases](upgrade_11gr2_53_sql_rep.md#idp844568) </span>

Replication can now be enabled on existing SQL databases, and replication is now disabled permanently instead of temporarily. Replication is enabled on an existing database the same way it is enabled on a new database, with one restriction. The existing database must configure itself as the initial master of a new replication group. To disable replication on a database permanently, use `pragma replication=OFF;`.

### PRAGMAs With Permanent Effects

- `pragma replication=OFF;`

### PRAGMAs That Can Now Operate on Existing Databases

- `pragma replication_local_site="host:port";`
- `pragma replication_initial_master=ON;`
- `pragma replication=ON;`
