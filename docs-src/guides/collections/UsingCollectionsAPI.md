---
title: "Using the DB Java Collections API"
api-name: "Using the DB Java Collections API"
source: docs/collections/tutorial/UsingCollectionsAPI.html
---
## Using the DB Java Collections API

<span class="sect2"> [Using Transactions](UsingCollectionsAPI.md#UsingTransactions) </span>

<span class="sect2"> [Transaction Rollback](UsingCollectionsAPI.md#TransactionRollback) </span>

<span class="sect2"> [Selecting Access Methods](UsingCollectionsAPI.md#SelectingAccessMethods) </span>

<span class="sect2"> [Access Method Restrictions](UsingCollectionsAPI.md#AccessMethodRestrictions) </span>

An <a href="../../java/com/sleepycat/db/Environment.html" class="ulink" target="_top">Environment</a> manages the resources for one or more data stores. A <a href="../../java/com/sleepycat/db/Database.html" class="ulink" target="_top">Database</a> object represents a single database and is created via a method on the environment object. <a href="../../java/com/sleepycat/db/SecondaryDatabase.html" class="ulink" target="_top">SecondaryDatabase</a> objects represent an index associated with a primary database. An access method must be chosen for each database and secondary database. Primary and secondary databases are then used to create stored collection objects, as described in <a href="UsingStoredCollections.md" class="xref" title="Using Stored Collections">Using Stored Collections</a> .

### Using Transactions

Once you have an environment, one or more databases, and one or more stored collections, you are ready to access (read and write) stored data. For a transactional environment, a transaction must be started before accessing data, and must be committed or aborted after access is complete. The DB Java Collections API provides several ways of managing transactions.

The recommended technique is to use the <a href="../../java/com/sleepycat/collections/TransactionRunner.html" class="ulink" target="_top">TransactionRunner</a> class along with your own implementation of the <a href="../../java/com/sleepycat/collections/TransactionWorker.html" class="ulink" target="_top">TransactionWorker</a> interface. <a href="../../java/com/sleepycat/collections/TransactionRunner.html" class="ulink" target="_top">TransactionRunner</a> will call your <a href="../../java/com/sleepycat/collections/TransactionWorker.html" class="ulink" target="_top">TransactionWorker</a> implementation class to perform the data access or work of the transaction. This technique has the following benefits:

- Transaction exceptions will be handled transparently and retries will be performed when deadlocks are detected.

- The transaction will automatically be committed if your <a href="../../java/com/sleepycat/collections/TransactionWorker.html#doWork()" class="ulink" target="_top">TransactionWorker.doWork()</a> method returns normally, or will be aborted if `doWork()` throws an exception.

- `TransactionRunner` can be used for non-transactional environments as well, allowing you to write your application independently of the environment.

If you don't want to use <a href="../../java/com/sleepycat/collections/TransactionRunner.html" class="ulink" target="_top">TransactionRunner</a>, the alternative is to use the <a href="../../java/com/sleepycat/collections/CurrentTransaction.html" class="ulink" target="_top">CurrentTransaction</a> class.

1.  Obtain a CurrentTransaction instance by calling the <a href="../../java/com/sleepycat/collections/CurrentTransaction.html#getInstance(com.sleepycat.db.Environment)" class="ulink" target="_top">CurrentTransaction.getInstance</a> method. The instance returned can be used by all threads in a program.

2.  Use <a href="../../java/com/sleepycat/collections/CurrentTransaction.html#beginTransaction(com.sleepycat.db.TransactionConfig)" class="ulink" target="_top">CurrentTransaction.beginTransaction()</a>, <a href="../../java/com/sleepycat/collections/CurrentTransaction.html#commitTransaction()" class="ulink" target="_top">CurrentTransaction.commitTransaction()</a> and <a href="../../java/com/sleepycat/collections/CurrentTransaction.html#abortTransaction()" class="ulink" target="_top">CurrentTransaction.abortTransaction()</a> to directly begin, commit and abort transactions.

If you choose to use CurrentTransaction directly you must handle the <a href="../../java/com/sleepycat/db/DeadlockException.html" class="ulink" target="_top">DeadlockException</a> exception and perform retries yourself. Also note that CurrentTransaction may only be used in a transactional environment.

The DB Java Collections API supports nested transactions. If <a href="../../java/com/sleepycat/collections/TransactionRunner.html#run(com.sleepycat.collections.TransactionWorker)" class="ulink" target="_top">TransactionRunner.run(com.sleepycat.collections.TransactionWorker)</a> or <a href="../../java/com/sleepycat/collections/CurrentTransaction.html#beginTransaction(com.sleepycat.db.TransactionConfig)" class="ulink" target="_top">CurrentTransaction.beginTransaction()</a> , is called while another transaction is active, a child transaction is created. When <a href="../../java/com/sleepycat/collections/TransactionRunner.html#run(com.sleepycat.collections.TransactionWorker)" class="ulink" target="_top">TransactionRunner.run(com.sleepycat.collections.TransactionWorker)</a> returns, or when <a href="../../java/com/sleepycat/collections/CurrentTransaction.html#commitTransaction()" class="ulink" target="_top">CurrentTransaction.commitTransaction()</a> or <a href="../../java/com/sleepycat/collections/CurrentTransaction.html#abortTransaction()" class="ulink" target="_top">CurrentTransaction.abortTransaction()</a> is called, the parent transaction becomes active again. Note that because only one transaction is active per-thread, it is impossible to accidentally use a parent transaction while a child transaction is active.

The DB Java Collections API supports transaction auto-commit. If no transaction is active and a write operation is requested for a transactional database, auto-commit is used automatically.

The DB Java Collections API also supports transaction dirty-read via the <a href="../../java/com/sleepycat/collections/StoredCollections.html" class="ulink" target="_top">StoredCollections</a> class. When dirty-read is enabled for a collection, data will be read that has been modified by another transaction but not committed. Using dirty-read can improve concurrency since reading will not wait for other transactions to complete. For a non-transactional container, dirty-read has no effect. See <a href="../../java/com/sleepycat/collections/StoredCollections.html" class="ulink" target="_top">StoredCollections</a> for how to create a dirty-read collection.

### Transaction Rollback

When a transaction is aborted (or rolled back) the application is responsible for discarding references to any data objects that were modified during the transaction. Since the DB Java Collections API treats data by value, not by reference, neither the data objects nor the DB Java Collections API objects contain status information indicating whether the data objects are 1- in sync with the database, 2- dirty (contain changes that have not been written to the database), 3- stale (were read previously but have become out of sync with changes made to the database), or 4- contain changes that cannot be committed because of an aborted transaction.

For example, a given data object will reflect the current state of the database after reading it within a transaction. If the object is then modified it will be out of sync with the database. When the modified object is written to the database it will then be in sync again. But if the transaction is aborted the object will then be out of sync with the database. References to objects for aborted transactions should no longer be used. When these objects are needed later they should be read fresh from the database.

When an existing stored object is to be updated, special care should be taken to read the data, then modify it, and then write it to the database, all within a single transaction. If a stale data object (an object that was read previously but has since been changed in the database) is modified and then written to the database, database changes may be overwritten unintentionally.

When an application enforces rules about concurrent access to specific data objects or all data objects, the rules described here can be relaxed. For example, if the application knows that a certain object is only modified in one place, it may be able to reliably keep a current copy of that object. In that case, it is not necessary to reread the object before updating it. That said, if arbitrary concurrent access is to be supported, the safest approach is to always read data before modifying it within a single transaction.

Similar concerns apply to using data that may have become stale. If the application depends on current data, it should be read fresh from the database just before it is used.

### Selecting Access Methods

For each data store and secondary index, you must choose from one of the access methods in the table below. The access method determines not only whether sorted keys or duplicate keys are supported, but also what types of collection views may be used and what restrictions are imposed on the collection views.

| Access Method | Ordered | Duplicates | Record Numbers | Database Type | `DatabaseConfig` Method |
|----|----|----|----|----|----|
| BTREE-UNIQUE | Yes | No | No | <a href="../../java/com/sleepycat/db/DatabaseType.html#BTREE" class="ulink" target="_top">BTREE</a> | None |
| BTREE-DUP | Yes | Yes, Unsorted | No | <a href="../../java/com/sleepycat/db/DatabaseType.html#BTREE" class="ulink" target="_top">BTREE</a> | <a href="../../java/com/sleepycat/db/DatabaseConfig.html#setUnsortedDuplicates(boolean)" class="ulink" target="_top">setUnsortedDuplicates</a> |
| BTREE-DUPSORT | Yes | Yes, Sorted | No | <a href="../../java/com/sleepycat/db/DatabaseType.html#BTREE" class="ulink" target="_top">BTREE</a> | <a href="../../java/com/sleepycat/db/DatabaseConfig.html#setSortedDuplicates(boolean)" class="ulink" target="_top">setSortedDuplicates</a> |
| BTREE-RECNUM | Yes | No | Yes, Renumbered | <a href="../../java/com/sleepycat/db/DatabaseType.html#BTREE" class="ulink" target="_top">BTREE</a> | <a href="../../java/com/sleepycat/db/DatabaseConfig.html#setBtreeRecordNumbers(boolean)" class="ulink" target="_top">setBtreeRecordNumbers</a> |
| HASH-UNIQUE | No | No | No | <a href="../../java/com/sleepycat/db/DatabaseType.html#HASH" class="ulink" target="_top">HASH</a> | None |
| HASH-DUP | No | Yes, Unsorted | No | <a href="../../java/com/sleepycat/db/DatabaseType.html#HASH" class="ulink" target="_top">HASH</a> | <a href="../../java/com/sleepycat/db/DatabaseConfig.html#setUnsortedDuplicates(boolean)" class="ulink" target="_top">setUnsortedDuplicates</a> |
| HASH-DUPSORT | No | Yes, Sorted | No | <a href="../../java/com/sleepycat/db/DatabaseType.html#HASH" class="ulink" target="_top">HASH</a> | <a href="../../java/com/sleepycat/db/DatabaseConfig.html#setSortedDuplicates(boolean)" class="ulink" target="_top">setSortedDuplicates</a> |
| QUEUE | Yes | No | Yes, Fixed | <a href="../../java/com/sleepycat/db/DatabaseType.html#QUEUE" class="ulink" target="_top">QUEUE</a> | None |
| RECNO | Yes | No | Yes, Fixed | <a href="../../java/com/sleepycat/db/DatabaseType.html#RECNO" class="ulink" target="_top">RECNO</a> | None |
| RECNO-RENUMBER | Yes | No | Yes, Renumbered | <a href="../../java/com/sleepycat/db/DatabaseType.html#RECNO" class="ulink" target="_top">RECNO</a> | <a href="../../java/com/sleepycat/db/DatabaseConfig.html#setRenumbering(boolean)" class="ulink" target="_top">setRenumbering</a> |

Please see <span class="html"><a href="../../guides/programmer_reference/am_conf.md#am_conf_intro" class="ulink" target="_top">Available Access Methods</a> in</span> the *Berkeley DB Programmer's Reference Guide* for more information on access method configuration.

### Access Method Restrictions

The restrictions imposed by the access method on the database model are:

- If keys are ordered then data may be enumerated in key order and key ranges may be used to form subsets of a data store. The `SortedMap` and `SortedSet` interfaces are supported for collections with ordered keys.

- If duplicates are allowed then more than one value may be associated with the same key. This means that the data store cannot be strictly considered a map — it is really a multi-map. See <a href="UsingStoredCollections.md" class="xref" title="Using Stored Collections">Using Stored Collections</a> for implications on the use of the collection interfaces.

- If duplicate keys are allowed for a data store then the data store may not have secondary indices.

- For secondary indices with duplicates, the duplicates must be sorted. This restriction is imposed by the DB Java Collections API.

- With sorted duplicates, all values for the same key must be distinct.

- If duplicates are unsorted, then values for the same key must be distinct.

- If record number keys are used, the the number of records is limited to the maximum value of an unsigned 32-bit integer.

- If record number keys are renumbered, then standard List add/remove behavior is supported but concurrency/performance is reduced.

See <a href="UsingStoredCollections.md" class="xref" title="Using Stored Collections">Using Stored Collections</a> for more information on how access methods impact the use of stored collections.
