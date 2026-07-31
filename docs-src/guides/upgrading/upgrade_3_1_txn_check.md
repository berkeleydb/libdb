---
title: "txn_checkpoint"
api-name: "txn_checkpoint"
source: docs/upgrading/upgrade_3_1_txn_check.html
---
## txn_checkpoint

An additional argument has been added to the txn_checkpoint function.

The application should be searched for any occurrences of txn_checkpoint. For each one, an argument of 0 should be appended to the current arguments.
