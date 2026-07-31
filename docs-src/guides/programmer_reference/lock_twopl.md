---
title: "Locking with transactions: two-phase locking"
api-name: "Locking with transactions: two-phase locking"
source: docs/programmer_reference/lock_twopl.html
---
## Locking with transactions: two-phase locking

Berkeley DB uses a locking protocol called <span class="emphasis">*two-phase locking (2PL)*</span>. This is the traditional protocol used in conjunction with lock-based transaction systems.

In a two-phase locking system, transactions are divided into two distinct phases. During the first phase, the transaction only acquires locks; during the second phase, the transaction only releases locks. More formally, once a transaction releases a lock, it may not acquire any additional locks. Practically, this translates into a system in which locks are acquired as they are needed throughout a transaction and retained until the transaction ends, either by committing or aborting. In Berkeley DB, locks are released during <a href="../../api/c/txnabort.md" class="olink">DB_TXN-&gt;abort()</a> or <a href="../../api/c/txncommit.md" class="olink">DB_TXN-&gt;commit()</a>. The only exception to this protocol occurs when we use lock-coupling to traverse a data structure. If the locks are held only for traversal purposes, it is safe to release locks before transactions commit or abort.

For applications, the implications of 2PL are that long-running transactions will hold locks for a long time. When designing applications, lock contention should be considered. In order to reduce the probability of deadlock and achieve the best level of concurrency possible, the following guidelines are helpful.

1.  When accessing multiple databases, design all transactions so that they access the files in the same order.
2.  If possible, access your most hotly contested resources last (so that their locks are held for the shortest time possible).
3.  If possible, use nested transactions to protect the parts of your transaction most likely to deadlock.
