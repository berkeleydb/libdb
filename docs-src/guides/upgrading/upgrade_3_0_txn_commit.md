---
title: "txn_commit"
api-name: "txn_commit"
source: docs/upgrading/upgrade_3_0_txn_commit.html
---
## txn_commit

An additional argument has been added to the txn_commit function.

The application should be searched for any occurrences of txn_commit. For each one, an argument of 0 should be appended to the current arguments.
