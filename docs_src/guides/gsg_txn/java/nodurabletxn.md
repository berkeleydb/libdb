---
title: "Non-Durable Transactions"
api-name: "Non-Durable Transactions"
source: docs/gsg_txn/JAVA/nodurabletxn.html
---
## Non-Durable Transactions

As previously noted, by default transaction commits are durable because they cause the modifications performed under the transaction to be synchronously recorded in your on-disk log files. However, it is possible to use non-durable transactions.

You may want non-durable transactions for performance reasons. For example, you might be using transactions simply for the isolation guarantee. In this case, you might not want a durability guarantee and so you may want to prevent the disk I/O that normally accompanies a transaction commit.

There are several ways to remove the durability guarantee for your transactions:

- Specify `true` to the `EnvironmentConfig.setTxnNoSync()` method. This causes DB to not synchronously force any log data to disk upon transaction commit. That is, the modifications are held entirely in the in-memory cache and the logging information is not forced to the filesystem for long-term storage. Note, however, that the logging data will eventually make it to the filesystem (assuming no application or OS crashes) as a part of DB's management of its logging buffers and/or cache.

  This form of a commit provides a weak durability guarantee because data loss can occur due to an application, JVM, or OS crash.

  This behavior is specified on a per-environment handle basis. In order for your application to exhibit consistent behavior, you need to specify this method for all of the environment handles used in your application.

  You can achieve this behavior on a transaction by transaction basis by using `Transaction.commitNoSync()` to commit your transaction, or by specifying `true` to the `TransactionConfig.setNoSync()` method when starting the transaction.

- Specify `true` to the `EnvironmentConfig.setTxnWriteNoSync()` method. This causes logging data to be synchronously written to the OS's file system buffers upon transaction commit. The data will eventually be written to disk, but this occurs when the operating system chooses to schedule the activity; the transaction commit can complete successfully before this disk I/O is performed by the OS.

  This form of commit protects you against application and JVM crashes, but not against OS crashes. This method offers less room for the possibility of data loss than does `EnvironmentConfig.setTxnNoSync()`.

  This behavior is specified on a per-environment handle basis. In order for your application to exhibit consistent behavior, you need to specify this method for all of the environment handles used in your application.

  You can achieve this behavior on a transaction by transaction basis by using `Transaction.commitWriteNoSync()` to commit your transaction, or by specifying `true` to `TransactionConfig.setWriteNoSync()` method when starting the transaction.

- Maintain your logs entirely in-memory. In this case, your logs are never written to disk. The result is that you lose all durability guarantees. See <a href="logconfig.md#inmemorylogging" class="xref" title="Configuring In-Memory Logging">Configuring In-Memory Logging</a> for more information.
