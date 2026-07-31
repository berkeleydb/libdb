---
title: "DB_TXN->commit()"
api-name: "DB_TXN->commit()"
source: docs/api_reference/C/txncommit.html
---
## DB_TXN-\>commit()

``` c
#include <db.h>

int
DB_TXN->commit(DB_TXN *tid, u_int32_t flags);  
```

The `DB_TXN->commit()` method ends the transaction.

In the case of nested transactions, if the transaction is a parent transaction, committing the parent transaction causes all unresolved children of the parent to be committed. In the case of nested transactions, if the transaction is a child transaction, its locks are not released, but are acquired by its parent. Although the commit of the child transaction will succeed, the actual resolution of the child transaction is postponed until the parent transaction is committed or aborted; that is, if its parent transaction commits, it will be committed; and if its parent transaction aborts, it will be aborted.

All cursors opened within the transaction must be closed before the transaction is committed. If they are not closed, they will be closed by this function. When the close operation for a cursor fails, the method returns a non-zero error value for the first instance of such an error, closes the rest of the cursors, and then aborts the transaction.

After `DB_TXN->commit()` has been called, regardless of its return, the <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a> handle may not be accessed again. If `DB_TXN->commit()` encounters an error, the transaction and all child transactions of the transaction are aborted.

The `DB_TXN->commit()` method returns a non-zero error value on failure and 0 on success. The errors values that this method returns include the error values of the `DBcursor->close()` method and the following:

#### DB_LOCK_DEADLOCK

A transactional database environment operation was selected to resolve a deadlock.

#### DB_LOCK_NOTGRANTED

A Berkeley DB Concurrent Data Store database environment configured for lock timeouts was unable to grant a lock in the allowed time.

You attempted to open a database handle that is configured for no waiting exclusive locking, but the exclusive lock could not be immediately obtained. See <a href="dbset_lk_exclusive.md" class="xref" title="DB-&gt;set_lk_exclusive()">DB-&gt;set_lk_exclusive()</a> for more information.

#### DB_REP_LEASE_EXPIRED

The operation failed because the site's replication master lease has expired.

#### EINVAL

If the cursor is already closed; or if an invalid flag value or parameter was specified.

### Parameters

#### flags

The **flags** parameter must be set to 0 or one of the following values:

- `DB_TXN_NOSYNC`

  Do not synchronously flush the log. This means the transaction will exhibit the ACI (atomicity, consistency, and isolation) properties, but not D (durability); that is, database integrity will be maintained, but it is possible that this transaction may be undone during recovery.

  This behavior may be set for a Berkeley DB environment using the <a href="envset_flags.md" class="xref" title="DB_ENV-&gt;set_flags()">DB_ENV-&gt;set_flags()</a> method or for a single transaction using the <a href="txnbegin.md" class="xref" title="DB_ENV-&gt;txn_begin()">DB_ENV-&gt;txn_begin()</a> method. Any value specified to this method overrides both of those settings.

- `DB_TXN_SYNC`

  Synchronously flush the log. This means the transaction will exhibit all of the ACID (atomicity, consistency, isolation, and durability) properties.

  This behavior is the default for Berkeley DB environments unless the <a href="envset_flags.md#envset_flags_DB_TXN_NOSYNC" class="link">DB_TXN_NOSYNC</a> flag was specified to the <a href="envset_flags.md" class="xref" title="DB_ENV-&gt;set_flags()">DB_ENV-&gt;set_flags()</a> method. This behavior may also be set for a single transaction using the <a href="txnbegin.md" class="xref" title="DB_ENV-&gt;txn_begin()">DB_ENV-&gt;txn_begin()</a> method. Any value specified to this method overrides both of those settings.

- `DB_TXN_WRITE_NOSYNC`

  Write but do not synchronously flush the log on transaction commit. This means that transactions exhibit the ACI (atomicity, consistency, and isolation) properties, but not D (durability); that is, database integrity will be maintained, but if the system fails, it is possible some number of the most recently committed transactions may be undone during recovery. The number of transactions at risk is governed by how often the system flushes dirty buffers to disk and how often the log is checkpointed.

  This form of commit protects you against application crashes, but not against OS crashes. This method offers less room for the possiblity of data loss than does `DB_TXN_NOSYNC`.

  This behavior may be set for a Berkeley DB environment using the <a href="envset_flags.md" class="xref" title="DB_ENV-&gt;set_flags()">DB_ENV-&gt;set_flags()</a> method or for a single transaction using the <a href="txnbegin.md" class="xref" title="DB_ENV-&gt;txn_begin()">DB_ENV-&gt;txn_begin()</a> method. Any value specified to this method overrides both of those settings.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a>

### See Also

<a href="txn.md#txnlist" class="xref" title="Transaction Subsystem and Related Methods">Transaction Subsystem and Related Methods</a>
