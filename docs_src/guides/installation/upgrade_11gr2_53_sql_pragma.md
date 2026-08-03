---
title: "New Berkeley DB SQL API PRAGMAs"
api-name: "New Berkeley DB SQL API PRAGMAs"
source: docs/installation/upgrade_11gr2_53_sql_pragma.html
---
## New Berkeley DB SQL API PRAGMAs

<span class="sect2"> [New PRAGMAs](upgrade_11gr2_53_sql_pragma.md#idp843792) </span>

Two new Berkeley DB SQL API specific pragmas have been added, `bdbsql_shared_resources` and `bdbsql_set_lock_tablesize`. `bdbsql_shared_resources` is used to set the maximum amount of memory, in bytes, to be used by shared structures in the main environment region, which is useful in applications with a large number of tables, transactions, or threads. `bdbsql_set_lock_tablesize` is used to set the number of buckets in the lock object hash table in the Berkeley DB environment, which is useful if an application has many concurrent long running transactions.

### New PRAGMAs

- `PRAGMA bdbsql_shared_resources[=N]`
- `PRAGMA bdbsql_set_lock_tablesize[=N]`
