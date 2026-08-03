---
title: "Degrees of isolation"
api-name: "Degrees of isolation"
source: docs/programmer_reference/transapp_read.html
---
## Degrees of isolation

<span class="sect2"> [Snapshot Isolation](transapp_read.md#snapshot_isolation) </span>

Transactions can be isolated from each other to different degrees. <span class="emphasis">*Serializable*</span> provides the most isolation, and means that, for the life of the transaction, every time a thread of control reads a data item, it will be unchanged from its previous value (assuming, of course, the thread of control does not itself modify the item). By default, Berkeley DB enforces serializability whenever database reads are wrapped in transactions. This is also known as <span class="emphasis">*degree 3 isolation*</span>.

Most applications do not need to enclose all reads in transactions, and when possible, transactionally protected reads at serializable isolation should be avoided as they can cause performance problems. For example, a serializable cursor sequentially reading each key/data pair in a database, will acquire a read lock on most of the pages in the database and so will gradually block all write operations on the databases until the transaction commits or aborts. Note, however, that if there are update transactions present in the application, the read operations must still use locking, and must be prepared to repeat any operation (possibly closing and reopening a cursor) that fails with a return value of <a href="program_errorret.md#program_errorret.DB_LOCK_DEADLOCK" class="link">DB_LOCK_DEADLOCK</a>. Applications that need repeatable reads are ones that require the ability to repeatedly access a data item knowing that it will not have changed (for example, an operation modifying a data item based on its existing value).

<span class="emphasis">*Snapshot isolation*</span> also guarantees repeatable reads, but avoids read locks by using multiversion concurrency control (MVCC). This makes update operations more expensive, because they have to allocate space for new versions of pages in cache and make copies, but avoiding read locks can significantly increase throughput for many applications. Snapshot isolation is discussed in detail below.

A transaction may only require <span class="emphasis">*cursor stability*</span>, that is only be guaranteed that cursors see committed data that does not change so long as it is addressed by the cursor, but may change before the reading transaction completes. This is also called <span class="emphasis">*degree 2 isolation*</span>. Berkeley DB provides this level of isolation when a transaction is started with the <a href="../../api/c/dbcget.md#dbcget_DB_READ_COMMITTED" class="olink">DB_READ_COMMITTED</a> flag. This flag may also be specified when opening a cursor within a fully isolated transaction.

Berkeley DB optionally supports reading uncommitted data; that is, read operations may request data which has been modified but not yet committed by another transaction. This is also called <span class="emphasis">*degree 1 isolation*</span>. This is done by first specifying the <a href="../../api/c/dbopen.md#dbopen_DB_READ_UNCOMMITTED" class="olink">DB_READ_UNCOMMITTED</a> flag when opening the underlying database, and then specifying the <a href="../../api/c/dbopen.md#dbopen_DB_READ_UNCOMMITTED" class="olink">DB_READ_UNCOMMITTED</a> flag when beginning a transaction, opening a cursor, or performing a read operation. The advantage of using <a href="../../api/c/dbopen.md#dbopen_DB_READ_UNCOMMITTED" class="olink">DB_READ_UNCOMMITTED</a> is that read operations will not block when another transaction holds a write lock on the requested data; the disadvantage is that read operations may return data that will disappear should the transaction holding the write lock abort.

### Snapshot Isolation

To make use of snapshot isolation, databases must first be configured for multiversion access by calling <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a> with the <a href="../../api/c/dbopen.md#dbopen_DB_MULTIVERSION" class="olink">DB_MULTIVERSION</a> flag. Then transactions or cursors must be configured with the <a href="../../api/c/txnbegin.md#txnbegin_DB_TXN_SNAPSHOT" class="olink">DB_TXN_SNAPSHOT</a> flag.

When configuring an environment for snapshot isolation, it is important to realize that having multiple versions of pages in cache means that the working set will take up more of the cache. As a result, snapshot isolation is best suited for use with larger cache sizes.

If the cache becomes full of page copies before the old copies can be discarded, additional I/O will occur as pages are written to temporary "freezer" files. This can substantially reduce throughput, and should be avoided if possible by configuring a large cache and keeping snapshot isolation transactions short. The amount of cache required to avoid freezing buffers can be estimated by taking a checkpoint followed by a call to <a href="../../api/c/logarchive.md" class="olink">DB_ENV-&gt;log_archive()</a>. The amount of cache required is approximately double the size of logs that remains.

The environment should also be configured for sufficient transactions using <a href="../../api/c/envset_tx_max.md" class="olink">DB_ENV-&gt;set_tx_max()</a>. The maximum number of transactions needs to include all transactions executed concurrently by the application plus all cursors configured for snapshot isolation. Further, the transactions are retained until the last page they created is evicted from cache, so in the extreme case, an additional transaction may be needed for each page in the cache. Note that cache sizes under 500MB are increased by 25%, so the calculation of number of pages needs to take this into account.

So when <span class="emphasis">*should*</span> applications use snapshot isolation?

- There is a large cache relative to size of updates performed by concurrent transactions; and
- Read/write contention is limiting the throughput of the application; or
- The application is all or mostly read-only, and contention for the lock manager mutex is limiting throughput.

The simplest way to take advantage of snapshot isolation is for queries: keep update transactions using full read/write locking and set <a href="../../api/c/txnbegin.md#txnbegin_DB_TXN_SNAPSHOT" class="olink">DB_TXN_SNAPSHOT</a> on read-only transactions or cursors. This should minimize blocking of snapshot isolation transactions and will avoid introducing new <a href="program_errorret.md#program_errorret.DB_LOCK_DEADLOCK" class="link">DB_LOCK_DEADLOCK</a> errors.

If the application has update transactions which read many items and only update a small set (for example, scanning until a desired record is found, then modifying it), throughput may be improved by running some updates at snapshot isolation as well.
