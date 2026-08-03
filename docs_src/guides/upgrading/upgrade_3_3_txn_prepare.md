---
title: "txn_prepare"
api-name: "txn_prepare"
source: docs/upgrading/upgrade_3_3_txn_prepare.html
---
## txn_prepare

An additional argument has been added to the txn_prepare function. If your application calls txn_prepare (that is, is performing two-phase commit using Berkeley DB as a local resource manager), see the section titled *Distributed Transactions* in versions of this book that existed prior to release 4.8.
