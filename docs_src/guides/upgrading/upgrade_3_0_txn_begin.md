---
title: "txn_begin"
api-name: "txn_begin"
source: docs/upgrading/upgrade_3_0_txn_begin.html
---
## txn_begin

An additional argument has been added to the txn_begin function.

The application should be searched for any occurrences of txn_begin. For each one, an argument of 0 should be appended to the current arguments.
