---
title: "Exclusive Transactions in the SQL Layer"
api-name: "Exclusive Transactions in the SQL Layer"
source: docs/installation/upgrade_11gr2_52_excl_txn_sql.html
---
## Exclusive Transactions in the SQL Layer

Issuing the SQL command `BEGIN TRANSACTION EXCLUSIVE` will now cause any other transactions accessing the database to block, or return a `SQLITE_BUSY` or `SQLITE_LOCK` error, until the exclusive transaction is committed or aborted. Previously, non-exclusive transactions could execute concurrently with an exclusive transaction.
