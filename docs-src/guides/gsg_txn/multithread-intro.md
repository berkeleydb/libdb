---
title: "Multi-threaded and Multi-process Applications"
api-name: "Multi-threaded and Multi-process Applications"
source: docs/gsg_txn/C/multithread-intro.html
---
## Multi-threaded and Multi-process Applications

DB is designed to support multi-threaded and multi-process applications, but their usage means you must pay careful attention to issues of concurrency. Transactions help your application's concurrency by providing various levels of isolation for your threads of control. In addition, DB provides mechanisms that allow you to detect and respond to deadlocks.

<span class="emphasis">*Isolation*</span> means that database modifications made by one transaction will not normally be seen by readers from another transaction until the first commits its changes. Different threads use different transaction handles, so this mechanism is normally used to provide isolation between database operations performed by different threads.

Note that DB supports different isolation levels. For example, you can configure your application to see uncommitted reads, which means that one transaction can see data that has been modified but not yet committed by another transaction. Doing this might mean your transaction reads data "dirtied" by another transaction, but which subsequently might change before that other transaction commits its changes. On the other hand, lowering your isolation requirements means that your application can experience improved throughput due to reduced lock contention.

For more information on concurrency, on managing isolation levels, and on deadlock detection, see <a href="txnconcurrency.md" class="xref" title="Chapter 4. Concurrency">Concurrency</a>.
