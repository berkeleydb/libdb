---
title: "Enabling Transaction Snapshots in the SQL Layer"
api-name: "Enabling Transaction Snapshots in the SQL Layer"
source: docs/installation/upgrade_11gr2_52_mvcc_sql.html
---
## Enabling Transaction Snapshots in the SQL Layer

<span class="sect2"> [New Pragmas](upgrade_11gr2_52_mvcc_sql.md#idp951464) </span>

Read/write concurrency can now be enabled in the SQL API by using `PRAGMA multiversion=on` before accessing any tables in the database. After multiversion has been enabled, it can be temporarily disabled using the `PRAGMA transaction_snapshots=on/off`.

### New Pragmas

For more details on pragmas concerning Transaction Snapshots read <a href="../../guides/bdb-sql/mvcc.md" class="olink">Using Multiversion Concurrency Control</a> in the *Berkeley DB Getting Started with the SQL APIs* guide.

- `PRAGMA multiversion=ON|OFF;`
- `PRAGMA snapshot_isolation=ON|OFF`
