---
title: "Enabling Replication in the SQL Layer"
api-name: "Enabling Replication in the SQL Layer"
source: docs/installation/upgrade_11gr2_52_rep_sql.html
---
## Enabling Replication in the SQL Layer

<span class="sect2"> [New Pragmas](upgrade_11gr2_52_rep_sql.md#idp962696) </span>

Replication can now be enabled and configured in the SQL layer using pragmas. The pragmas `replication_local_site`, `replication_initial_master`, and `replication_remote_site` can be used to configure the replication group. Note that when the BDB SQL replicated application is initially started, a specific master site must be explicitly designated. After configuring the replication group, start replication using `PRAGMA replication=ON`.

To display replication statistics in the dbsql shell, use:

``` c
dbsql> .stat :rep
```

### New Pragmas

For more details on the replication pragmas see <a href="../../guides/bdb-sql/reppragma.md" class="olink">Replication PRAGMAs</a> in the *Berkeley DB Getting Started with the SQL APIs* guide.

- `PRAGMA replication=ON|OFF`
- `PRAGMA replication_initial_master=ON|OFF`
- `PRAGMA replication_local_site="hostname:port"`
- `PRAGMA replication_remote_site="hostname:port"`
- `PRAGMA replication_remove_site="host:port"`
- `PRAGMA replication_verbose_output=ON|OFF`
- `PRAGMA replication_verbose_file=filename`
