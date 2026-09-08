---
title: "DB_TXN->prepare()"
api-name: "DB_TXN->prepare()"
source: docs/api_reference/C/txnprepare.html
---
## DB_TXN-\>prepare()

``` c
#include <db.h>

int
DB_TXN->prepare(DB_TXN *tid, u_int8_t gid[DB_GID_SIZE]);  
```

The `DB_TXN->prepare()` method initiates the beginning of a two-phase commit.

In a distributed transaction environment, Berkeley DB can be used as a local transaction manager. In this case, the distributed transaction manager must send <span class="emphasis">*prepare*</span> messages to each local manager. The local manager must then issue a `DB_TXN->prepare()` and await its successful return before responding to the distributed transaction manager. Only after the distributed transaction manager receives successful responses from all of its <span class="emphasis">*prepare*</span> messages should it issue any <span class="emphasis">*commit*</span> messages.

In the case of nested transactions, preparing the parent causes all unresolved children of the parent transaction to be committed. Child transactions should never be explicitly prepared. Their fate will be resolved along with their parent's during global recovery.

All open cursors in the transaction are closed and the first cursor close error will be returned.

A transaction running under serializable snapshot isolation — one begun with the <a href="txnbegin.md#txnbegin_DB_TXN_SNAPSHOT" class="olink">DB_TXN_SNAPSHOT</a> flag, or begun in an environment configured with `DB_TXN_SNAPSHOT` — **cannot be prepared**: `DB_TXN->prepare()` returns `EINVAL` for such a transaction. SSI's serializability check runs at commit time and can still abort the transaction after prepare would have returned, so the combination is refused rather than allowing a prepared transaction to become uncommittable. Applications that need two-phase commit must run those transactions without `DB_TXN_SNAPSHOT`.

The `DB_TXN->prepare()` method returns a non-zero error value on failure and 0 on success. The errors that this method returns include the error values of `DBcursor->close()` and the following:

#### DB_LOCK_DEADLOCK

A transactional database environment operation was selected to resolve a deadlock.

#### DB_LOCK_NOTGRANTED

A Berkeley DB Concurrent Data Store database environment configured for lock timeouts was unable to grant a lock in the allowed time.

You attempted to open a database handle that is configured for no waiting exclusive locking, but the exclusive lock could not be immediately obtained. See <a href="dbset_lk_exclusive.md" class="xref" title="DB-&gt;set_lk_exclusive()">DB-&gt;set_lk_exclusive()</a> for more information.

#### EINVAL

If the transaction was begun with <a href="txnbegin.md#txnbegin_DB_TXN_SNAPSHOT" class="olink">DB_TXN_SNAPSHOT</a> (serializable snapshot isolation), which cannot be prepared for two-phase commit; if the cursor is already closed; or if an invalid flag value or parameter was specified.

### Parameters

#### gid

The **gid** parameter specifies the global transaction ID by which this transaction will be known. This global transaction ID will be returned in calls to <a href="txnrecover.md" class="xref" title="DB_ENV-&gt;txn_recover()">DB_ENV-&gt;txn_recover()</a> telling the application which global transactions must be resolved.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a>

### See Also

<a href="txn.md#txnlist" class="xref" title="Transaction Subsystem and Related Methods">Transaction Subsystem and Related Methods</a>
