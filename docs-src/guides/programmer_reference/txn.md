---
title: "Chapter 19.  The Transaction Subsystem"
api-name: "Chapter 19.  The Transaction Subsystem"
source: docs/programmer_reference/txn.html
---
## Chapter 19.  The Transaction Subsystem

**Table of Contents**

<span class="sect1"> [Introduction to the transaction subsystem](txn.md#txn_intro) </span>

<span class="sect1"> [Configuring transactions](txn_config.md) </span>

<span class="sect1"> [Transaction limits](txn_limits.md) </span>

<span class="sect2"> [Transaction IDs](txn_limits.md#idp53352320) </span>

<span class="sect2"> [Cursors](txn_limits.md#idp53275624) </span>

<span class="sect2"> [Multiple Threads of Control](txn_limits.md#idp53223368) </span>

## Introduction to the transaction subsystem

The Transaction subsystem makes operations atomic, consistent, isolated, and durable in the face of system and application failures. The subsystem requires that the data be properly logged and locked in order to attain these properties. Berkeley DB contains all the components necessary to transaction-protect the Berkeley DB access methods, and other forms of data may be protected if they are logged and locked appropriately.

The Transaction subsystem is created, initialized, and opened by calls to <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> with the <a href="../../api/c/envopen.md#envopen_DB_INIT_TXN" class="olink">DB_INIT_TXN</a> flag specified. Note that enabling transactions automatically enables logging, but does not enable locking because a single thread of control that needed atomicity and recoverability would not require it.

The <a href="../../api/c/txnbegin.md" class="olink">DB_ENV-&gt;txn_begin()</a> function starts a transaction, returning an opaque handle to a transaction. If the parent parameter to <a href="../../api/c/txnbegin.md" class="olink">DB_ENV-&gt;txn_begin()</a> is non-NULL, the new transaction is a child of the designated parent transaction.

The <a href="../../api/c/txnabort.md" class="olink">DB_TXN-&gt;abort()</a> function ends the designated transaction and causes all updates performed by the transaction to be undone. The end result is that the database is left in a state identical to the state that existed prior to the <a href="../../api/c/txnbegin.md" class="olink">DB_ENV-&gt;txn_begin()</a>. If the aborting transaction has any child transactions associated with it (even ones that have already been committed), they are also aborted. Any transactions that are unresolved (neither committed nor aborted) when the application or system fails are aborted during recovery.

The <a href="../../api/c/txncommit.md" class="olink">DB_TXN-&gt;commit()</a> function ends the designated transaction and makes all the updates performed by the transaction permanent, even in the face of application or system failure. If this is a parent transaction committing, all child transactions that individually committed or had not been resolved are also committed.

Transactions are identified by 32-bit unsigned integers. The ID associated with any transaction can be obtained using the <a href="../../api/c/txnid.md" class="olink">DB_TXN-&gt;id()</a> function. If an application is maintaining information outside of Berkeley DB it wants to transaction-protect, it should use this transaction ID as the locking ID.

The <a href="../../api/c/txncheckpoint.md" class="olink">DB_ENV-&gt;txn_checkpoint()</a> function causes a transaction checkpoint. A checkpoint is performed using to a specific log sequence number (LSN), referred to as the checkpoint LSN. When a checkpoint completes successfully, it means that all data buffers whose updates are described by LSNs less than the checkpoint LSN have been written to disk. This, in turn, means that the log records less than the checkpoint LSN are no longer necessary for normal recovery (although they would be required for catastrophic recovery if the database files were lost), and all log files containing only records prior to the checkpoint LSN may be safely archived and removed.

The time required to run normal recovery is proportional to the amount of work done between checkpoints. If a large number of modifications happen between checkpoints, many updates recorded in the log may not have been written to disk when failure occurred, and recovery may take longer to run. Generally, if the interval between checkpoints is short, data may be being written to disk more frequently, but the recovery time will be shorter. Often, the checkpoint interval is tuned for each specific application.

The <a href="../../api/c/txnstat.md" class="olink">DB_TXN-&gt;stat()</a> method returns information about the status of the transaction subsystem. It is the programmatic interface used by the <a href="../../api/c/db_stat.md" class="olink">db_stat</a> utility.

The transaction system is closed by a call to <a href="../../api/c/envclose.md" class="olink">DB_ENV-&gt;close()</a>.

Finally, the entire transaction system may be removed using the <a href="../../api/c/envremove.md" class="olink">DB_ENV-&gt;remove()</a> method.

For more information on the transaction subsystem methods, see the <a href="../../api/c/txn.md#txnlist" class="olink">Transaction Subsystem and Related Methods</a> section in the *Berkeley DB C API Reference Guide.*
