---
title: "Aborting a Transaction"
api-name: "Aborting a Transaction"
source: docs/gsg_txn/C/abortresults.html
---
## Aborting a Transaction

When you abort a transaction, all database modifications performed under the protection of the transaction are discarded, and all locks currently held by the transaction are released. In this event, your data is simply left in the state that it was in before the transaction began performing data modifications.

Once you have aborted a transaction, the transaction handle that you used for the transaction is no longer valid. To perform database activities under the control of a new transaction, you must obtain a fresh transactional handle.

To abort a transaction, call `DB_TXN->abort()`.
