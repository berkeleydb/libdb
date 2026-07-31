---
title: "DB_TXN->discard()"
api-name: "DB_TXN->discard()"
source: docs/api_reference/C/txndiscard.html
---
## DB_TXN-\>discard()

``` c
#include <db.h>

int
DB_TXN->discard(DB_TXN *tid, u_int32_t flags);  
```

The `DB_TXN->discard()` method frees up all the per-process resources associated with the specified <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a> handle, neither committing nor aborting the transaction. This call may be used only after calls to <a href="txnrecover.md" class="xref" title="DB_ENV-&gt;txn_recover()">DB_ENV-&gt;txn_recover()</a> when there are multiple global transaction managers recovering transactions in a single Berkeley DB environment. Any transactions returned by <a href="txnrecover.md" class="xref" title="DB_ENV-&gt;txn_recover()">DB_ENV-&gt;txn_recover()</a> that are not handled by the current global transaction manager should be discarded using `DB_TXN->discard()`.

All open cursors in the transaction are closed and the first cursor close error, if any, is returned.

The `DB_TXN->discard()` method returns a non-zero error value on failure and 0 on success. The errors values that this method returns include the error values of `DBcursor->close()` and the following:

#### DB_LOCK_DEADLOCK

A transactional database environment operation was selected to resolve a deadlock.

#### DB_LOCK_NOTGRANTED

A Berkeley DB Concurrent Data Store database environment configured for lock timeouts was unable to grant a lock in the allowed time.

You attempted to open a database handle that is configured for no waiting exclusive locking, but the exclusive lock could not be immediately obtained. See <a href="dbset_lk_exclusive.md" class="xref" title="DB-&gt;set_lk_exclusive()">DB-&gt;set_lk_exclusive()</a> for more information.

#### EINVAL

If the cursor is already closed; or if an invalid flag value or parameter was specified.

After `DB_TXN->discard()` has been called, regardless of its return, the <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a> handle may not be accessed again.

### Parameters

#### flags

The **flags** parameter is currently unused, and must be set to 0.

### Errors

The `DB_TXN->discard()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the transaction handle does not refer to a transaction that was recovered into a prepared but not yet completed state; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a>

### See Also

<a href="txn.md#txnlist" class="xref" title="Transaction Subsystem and Related Methods">Transaction Subsystem and Related Methods</a>
