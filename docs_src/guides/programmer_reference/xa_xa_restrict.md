---
title: "Restrictions on XA Transactions"
api-name: "Restrictions on XA Transactions"
source: docs/programmer_reference/xa_xa_restrict.html
---
## Restrictions on XA Transactions

When you are using Berkeley DB for XA transactions, there are a few restrictions you should be aware of:

- Configure environment using the <a href="../../api/c/configuration_reference.md" class="olink">DB_CONFIG</a> file

  For most options, you must configure your environment via the <a href="../../api/c/configuration_reference.md" class="olink">DB_CONFIG</a> file because an XA application or server cannot control the environment creation.

- Snapshot isolation must be configured for the entire environment.

  Transactions managed by the Berkeley DB X/open compliant XA resource manager can be configured for transaction snapshots using either database open flags or the <a href="../../api/c/configuration_reference.md" class="olink">DB_CONFIG</a> file file. To configure using database open flags, open the XA managed database with the flag <a href="../../api/c/dbopen.md#dbopen_DB_MULTIVERSION" class="olink">DB_MULTIVERSION</a>. When using DB_CONFIG, include both of the following lines:

  ``` c
  set_flags DB_MULTIVERSION
  set_flags DB_TXN_SNAPSHOT
  ```

  Note that both methods will results in all transactions using transaction snapshots, there is no way to enable transaction snapshots in just a subset of XA managed transactions.

- No in-memory logging

  Upon return from xa_open, Berkeley DB checks to ensure there is no in-memory logging. If in-memory logging is detected, a FAILURE message is returned to the application.

- No application-level child transactions

  Berkeley DB verifies in the xa_start and xa_end calls that no XA transaction has a parent. If application-level child transactions are detected, a FAILURE message is returned to the application.

- All database-level operations, such as create, rename, and remove, must be performed in local BDB transactions, not distributed XA transactions

  Berkeley DB checks that there is no XA transaction currently active during these operations, and if detected, a FAILURE message is returned to the application.

- Close cursors before a service invocation returns

  Berkeley DB checks in the `xa_end` call that the `DB_TXN` has no active cursors open and and if detected, a FAILURE message is returned to the application.
