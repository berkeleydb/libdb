---
title: "DB_TXN->set_timeout()"
api-name: "DB_TXN->set_timeout()"
source: docs/api_reference/C/txnset_timeout.html
---
## DB_TXN-\>set_timeout()

``` c
#include <db.h>

u_int32_t
DB_TXN->set_timeout(DB_TXN *tid, db_timeout_t timeout, u_int32_t flags);  
```

The `DB_TXN->set_timeout()` method sets timeout values for locks or transactions for the specified transaction.

Timeouts are checked whenever a thread of control blocks on a lock or when deadlock detection is performed. In the case of `DB_SET_LOCK_TIMEOUT`, the timeout is for any single lock request. In the case of `DB_SET_TXN_TIMEOUT`, the timeout is for the life of the transaction. As timeouts are only checked when the lock request first blocks or when deadlock detection is performed, the accuracy of the timeout depends on how often deadlock detection is performed.

Timeout values may be specified for the database environment as a whole. Also, the database environment must enable the locking subsystem before timeout values can be specified. See <a href="envset_timeout.md" class="xref" title="DB_ENV-&gt;set_timeout()">DB_ENV-&gt;set_timeout()</a> for more information.

The `DB_TXN->set_timeout()` method configures operations performed on the underlying transaction, not only operations performed using the specified <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a> handle.

The `DB_TXN->set_timeout()` method may be called at any time during the life of the application.

The `DB_TXN->set_timeout()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### timeout

The **timeout** parameter is specified as an unsigned 32-bit number of microseconds, limiting the maximum timeout to roughly 71 minutes. A value of 0 disables timeouts for the transaction.

#### flags

The **flags** parameter must be set to one of the following values:

- `DB_SET_LOCK_TIMEOUT`

  Set the timeout value for locks in this transaction.

- `DB_SET_TXN_TIMEOUT`

  Set the timeout value for this transaction.

### Errors

The `DB_TXN->set_timeout()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a>

### See Also

<a href="txn.md#txnlist" class="xref" title="Transaction Subsystem and Related Methods">Transaction Subsystem and Related Methods</a>
