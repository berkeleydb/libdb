---
title: "Application Requirements"
api-name: "Application Requirements"
source: docs/gsg_txn/C/apireq.html
---
## Application Requirements

In order to use transactions, your application has certain requirements beyond what is required of non-transactional protected applications. They are:

- Environments.

  Environments are optional for non-transactional applications, but they are required for transactional applications.

  Environment usage is described in detail in <a href="usingtxns.md" class="xref" title="Chapter 3. Transaction Basics">Transaction Basics</a>.

- Transaction subsystem.

  In order to use transactions, you must explicitly enable the transactional subsystem for your application, and this must be done at the time that your environment is first created.

- Logging subsystem.

  The logging subsystem is required for recovery purposes, but its usage also means your application may require a little more administrative effort than it does when logging is not in use. See <a href="filemanagement.md" class="xref" title="Chapter 5. Managing DB Files">Managing DB Files</a> for more information.

- DB_TXN handles.

  In order to obtain the atomicity guarantee offered by the transactional subsystem (that is, combine multiple operations in a single unit of work), your application must use transaction handles. These handles are obtained from your DB_ENV objects. They should normally be short-lived, and their usage is reasonably simple. To complete a transaction and save the work it performed, you call its `commit()` method. To complete a transaction and discard its work, you call its `abort()` method.

  In addition, it is possible to use auto commit if you want to transactional protect a single write operation. Auto commit allows a transaction to be used without obtaining an explicit transaction handle. See <a href="autocommit.md" class="xref" title="Auto Commit">Auto Commit</a> for information on how to use auto commit.

- Database open requirements.

  In addition to using environments and initializing the correct subsystems, your application must transaction protect the database opens, and any secondary index associations, if subsequent operations on the databases are to be transaction protected. The database open and secondary index association are commonly transaction protected using auto commit.

- Deadlock detection.

  Typically transactional applications use multiple threads of control when accessing the database. Any time multiple threads are used on a single resource, the potential for lock contention arises. In turn, lock contention can lead to deadlocks. See <a href="blocking_deadlocks.md" class="xref" title="Locks, Blocks, and Deadlocks">Locks, Blocks, and Deadlocks</a> for more information.

  Therefore, transactional applications must frequently include code for detecting and responding to deadlocks. Note that this requirement is not <span class="emphasis">*specific*</span> to transactions – you can certainly write concurrent non-transactional DB applications. Further, not every transactional application uses concurrency and so not every transactional application must manage deadlocks. Still, deadlock management is so frequently a characteristic of transactional applications that we discuss it in this book. See <a href="txnconcurrency.md" class="xref" title="Chapter 4. Concurrency">Concurrency</a> for more information.
