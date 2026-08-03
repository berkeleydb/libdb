---
title: "DB_TXN->abort()"
api-name: "DB_TXN->abort()"
source: docs/api_reference/C/txnabort.html
---
## DB_TXN-\>abort()

``` c
#include <db.h>

int
DB_TXN->abort(DB_TXN *tid);  
```

The `DB_TXN->abort()` method causes an abnormal termination of the transaction. The log is played backward, and any necessary undo operations are done through the **tx_recover** function specified to <a href="envset_app_dispatch.md" class="xref" title="DB_ENV-&gt;set_app_dispatch()">DB_ENV-&gt;set_app_dispatch()</a>. Before `DB_TXN->abort()` returns, any locks held by the transaction will have been released.

In the case of nested transactions, aborting a parent transaction causes all children (unresolved or not) of the parent transaction to be aborted.

All cursors opened within the transaction must be closed before the transaction is aborted. If they are not closed, they will be closed by this function. If a close operation fails, the rest of the cursors are closed, and the database environment is set to the panic state.

After `DB_TXN->abort()` has been called, regardless of its return, the <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a> handle may not be accessed again.

The `DB_TXN->abort()` method returns a non-zero error value on failure and 0 on success.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a>

### See Also

<a href="txn.md#txnlist" class="xref" title="Transaction Subsystem and Related Methods">Transaction Subsystem and Related Methods</a>
