---
title: "Transaction limits"
api-name: "Transaction limits"
source: docs/programmer_reference/txn_limits.html
---
## Transaction limits

<span class="sect2"> [Transaction IDs](txn_limits.md#idp53352320) </span>

<span class="sect2"> [Cursors](txn_limits.md#idp53275624) </span>

<span class="sect2"> [Multiple Threads of Control](txn_limits.md#idp53223368) </span>

### Transaction IDs

Transactions are identified by 31-bit unsigned integers, which means there are just over two billion unique transaction IDs. When a database environment is initially created or recovery is run, the transaction ID name space is reset, and new transactions are numbered starting from 0x80000000 (2,147,483,648). The IDs will wrap if the maximum transaction ID is reached, starting again from 0x80000000. The most recently allocated transaction ID is the **st_last_txnid** value in the transaction statistics information, and can be displayed by the <a href="../../api/c/db_stat.md" class="olink">db_stat</a> utility.

### Cursors

When using transactions, cursors are localized to a single transaction. That is, a cursor may not span transactions, and must be opened and closed within a single transaction. In addition, intermingling transaction-protected cursor operations and non-transaction-protected cursor operations on the same database in a single thread of control is practically guaranteed to deadlock because the locks obtained for transactions and non-transactions can conflict.

### Multiple Threads of Control

Because transactions must hold all their locks until commit, a single transaction may accumulate a large number of long-term locks during its lifetime. As a result, when two concurrently running transactions access the same database, there is strong potential for conflict. Although Berkeley DB allows an application to have multiple outstanding transactions active within a single thread of control, great care must be taken to ensure that the transactions do not block each other (for example, attempt to obtain conflicting locks on the same data). If two concurrently active transactions in the same thread of control do encounter a lock conflict, the thread of control will deadlock so that the deadlock detector cannot detect the problem. In this case, there is no true deadlock, but because the transaction on which a transaction is waiting is in the same thread of control, no forward progress can be made.
