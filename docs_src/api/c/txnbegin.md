---
title: "DB_ENV->txn_begin()"
api-name: "DB_ENV->txn_begin()"
source: docs/api_reference/C/txnbegin.html
---
## DB_ENV-\>txn_begin()

``` c
#include <db.h>

int
DB_ENV->txn_begin(DB_ENV *env,
    DB_TXN *parent, DB_TXN **tid, u_int32_t flags);  
```

The `DB_ENV->txn_begin()` method creates a new transaction in the environment and copies a pointer to a <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a> that uniquely identifies it into the memory to which **tid** refers. Calling the <a href="txnabort.md" class="xref" title="DB_TXN-&gt;abort()">DB_TXN-&gt;abort()</a>, <a href="txncommit.md" class="xref" title="DB_TXN-&gt;commit()">DB_TXN-&gt;commit()</a> or <a href="txndiscard.md" class="xref" title="DB_TXN-&gt;discard()">DB_TXN-&gt;discard()</a> methods will discard the returned handle.

### Note

Transactions may only span threads if they do so serially; that is, each transaction must be active in only a single thread of control at a time. This restriction holds for parents of nested transactions as well; no two children may be concurrently active in more than one thread of control at any one time.

### Note

Cursors may not span transactions; that is, each cursor must be opened and closed within a single transaction.

### Note

A parent transaction may not issue any Berkeley DB operations — except for `DB_ENV->txn_begin()`, <a href="txnabort.md" class="xref" title="DB_TXN-&gt;abort()">DB_TXN-&gt;abort()</a> and <a href="txncommit.md" class="xref" title="DB_TXN-&gt;commit()">DB_TXN-&gt;commit()</a> — while it has active child transactions (child transactions that have not yet been committed or aborted).

The `DB_ENV->txn_begin()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### parent

If the **parent** parameter is non-NULL, the new transaction will be a nested transaction, with the transaction indicated by **parent** as its parent. Transactions may be nested to any level. In the presence of distributed transactions and two-phase commit, only the parental transaction, that is a transaction without a **parent** specified, should be passed as an parameter to <a href="txnprepare.md" class="xref" title="DB_TXN-&gt;prepare()">DB_TXN-&gt;prepare()</a>.

#### flags

The **flags** parameter must be set to 0 or by bitwise inclusively **OR**'ing together one or more of the following values:

- `DB_READ_COMMITTED`

  This transaction will have degree 2 isolation. This provides for cursor stability but not repeatable reads. Data items which have been previously read by this transaction may be deleted or modified by other transactions before this transaction completes.

- `DB_READ_UNCOMMITTED`

  This transaction will have degree 1 isolation. Read operations performed by the transaction may read modified but not yet committed data. Silently ignored if the `DB_READ_UNCOMMITTED` flag was not specified when the underlying database was opened.

- `DB_TXN_BULK`

  Enable transactional bulk insert optimization. When this flag is set, the transaction avoids logging the contents of insertions on newly allocated database pages. In a transaction that inserts a large number of new records, the I/O savings of choosing this option can be significant.

  Users of this option should be aware of several issues. When the optimization is in effect, page allocations that extend the database file are logged as usual; this allows transaction aborts to work correctly, both online and during recovery. At commit time, the database's pages are flushed to disk, eliminating the need to roll-forward the transaction during normal recovery. However, there are other recovery operations that depend on roll-forward, and care must be taken when `DB_TXN_BULK` transactions interact with them.

  In particular, `DB_TXN_BULK` is incompatible with replication, and is simply ignored when replication is enabled. Also, hot backup procedures must follow a particular protocol, introduced in Berkeley DB 11gR2.5.1, which is to set the <a href="envset_flags.md#set_flags_DB_HOTBACKUP_IN_PROGRESS" class="xref"><code class="literal">DB_HOTBACKUP_IN_PROGRESS</code></a> flag in the environment before starting to copy files. It is important to note that incremental hot backups can be invalidated by use of the bulk insert optimization. For more information, see the section on Hot Backup in the <span class="emphasis">*Getting Started With Transaction Processing Guide*</span> and the description of the flag <a href="envset_flags.md#set_flags_DB_HOTBACKUP_IN_PROGRESS" class="xref"><code class="literal">DB_HOTBACKUP_IN_PROGRESS</code></a> in `DB_ENV->set_flags`.

  The bulk insert optimization is effective only for top-level transactions. The `DB_TXN_BULK` flag is ignored when **parent** is non-null.

- `DB_TXN_FAMILY`

  Start this transaction as part of a transaction family: a group of transactions that share locks and never conflict with one another. A family transaction is read-only from the caller's perspective and does not itself perform updates; it is used to hand a common locking context to a set of cooperating transactions (for example, a client/server design that spreads one logical unit of work across several transaction handles). When this flag is set the transaction is created read-only and shares the locker of its family, so operations performed under different members of the same family do not deadlock against each other.

- `DB_TXN_NOSYNC`

  Do not synchronously flush the log when this transaction commits or prepares. This means the transaction will exhibit the ACI (atomicity, consistency, and isolation) properties, but not D (durability); that is, database integrity will be maintained but it is possible that this transaction may be undone during recovery.

  This behavior may be set for a Berkeley DB environment using the <a href="envset_flags.md" class="xref" title="DB_ENV-&gt;set_flags()">DB_ENV-&gt;set_flags()</a> method. Any value specified to this method overrides that setting.

- `DB_TXN_NOWAIT`

  If a lock is unavailable for any Berkeley DB operation performed in the context of this transaction, cause the operation to return <a href="../../guides/programmer_reference/program_errorret.md#program_errorret.DB_LOCK_DEADLOCK" class="olink">DB_LOCK_DEADLOCK</a> (or <a href="../../guides/programmer_reference/program_errorret.md#program_errorret.DB_LOCK_NOTGRANTED" class="olink">DB_LOCK_NOTGRANTED</a> if the database environment has been configured using the <a href="envset_flags.md#envset_flags_DB_TIME_NOTGRANTED" class="link">DB_TIME_NOTGRANTED</a> flag).

  This behavior may be set for a Berkeley DB environment using the <a href="envset_flags.md" class="xref" title="DB_ENV-&gt;set_flags()">DB_ENV-&gt;set_flags()</a> method. Any value specified to this method overrides that setting.

- `DB_TXN_SNAPSHOT`

  This transaction will execute with **serializable snapshot isolation** (SSI): snapshot isolation plus serializable conflict detection (the Cahill SSI algorithm). For databases with the <a href="dbopen.md#dbopen_DB_MULTIVERSION" class="link">DB_MULTIVERSION</a> flag set, data values are read as they are when the transaction begins, without taking read locks; for databases without DB_MULTIVERSION, read locks are acquired. On top of that consistent snapshot, Berkeley DB tracks read/write anti-dependencies between concurrent snapshot transactions and, when it detects a dependency structure that could produce a non-serializable schedule, aborts one of the transactions so the committed history is equivalent to some serial order.

  A transaction that hits such a potential anomaly is aborted with one of two Berkeley DB-specific error returns instead of committing: <a href="../../guides/programmer_reference/program_errorret.md" class="olink">DB_SNAPSHOT_UNSAFE</a> (a potential serializable-snapshot anomaly was detected via a read/write anti-dependency) or <a href="../../guides/programmer_reference/program_errorret.md" class="olink">DB_SNAPSHOT_CONFLICT</a> (a conflicting snapshot update was detected). On either return the application must abort the transaction and may retry it. The error `DB_LOCK_DEADLOCK` may also be returned from update operations if a snapshot transaction attempts to update data modified after it read the snapshot.

  A `DB_TXN_SNAPSHOT` (SSI) transaction **cannot be prepared for two-phase commit**: <a href="txnprepare.md" class="xref" title="DB_TXN-&gt;prepare()">DB_TXN-&gt;prepare()</a> returns `EINVAL` for such a transaction, because SSI's conflict status is not frozen at prepare time and a later-detected anomaly could not be honored after the transaction had entered the prepared state.

  > **Note:** In this fork, `DB_TXN_SNAPSHOT` provides *serializable* snapshot isolation. In stock Oracle Berkeley DB, `DB_TXN_SNAPSHOT` provided only plain (non-serializable) snapshot isolation, and the earlier `DB_TXN_SNAPSHOT_SAFE` flag has been removed — there is no separate non-serializable snapshot mode.

- `DB_TXN_SYNC`

  Synchronously flush the log when this transaction commits or prepares. This means the transaction will exhibit all of the ACID (atomicity, consistency, isolation, and durability) properties.

  This behavior is the default for Berkeley DB environments unless the `DB_TXN_NOSYNC` flag was specified to the <a href="envset_flags.md" class="xref" title="DB_ENV-&gt;set_flags()">DB_ENV-&gt;set_flags()</a> method. Any value specified to this method overrides that setting.

- `DB_TXN_WAIT`

  If a lock is unavailable for any Berkeley DB operation performed in the context of this transaction, wait for the lock.

  This behavior is the default for Berkeley DB environments unless the `DB_TXN_NOWAIT` flag was specified to the <a href="envset_flags.md" class="xref" title="DB_ENV-&gt;set_flags()">DB_ENV-&gt;set_flags()</a> method. Any value specified to this method overrides that setting.

- `DB_TXN_WRITE_NOSYNC`

  Write, but do not synchronously flush, the log when this transaction commits. This means the transaction will exhibit the ACI (atomicity, consistency, and isolation) properties, but not D (durability); that is, database integrity will be maintained, but if the system fails, it is possible some number of the most recently committed transactions may be undone during recovery. The number of transactions at risk is governed by how often the system flushes dirty buffers to disk and how often the log is flushed or checkpointed.

  This behavior may be set for a Berkeley DB environment using the <a href="envset_flags.md" class="xref" title="DB_ENV-&gt;set_flags()">DB_ENV-&gt;set_flags()</a> method. Any value specified to this method overrides that setting.

### Errors

The `DB_ENV->txn_begin()` method may fail and return one of the following non-zero errors:

#### ENOMEM

The maximum number of concurrent transactions has been reached.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a>

### See Also

<a href="txn.md#txnlist" class="xref" title="Transaction Subsystem and Related Methods">Transaction Subsystem and Related Methods</a>
