---
title: "DB_TXN->id()"
api-name: "DB_TXN->id()"
source: docs/api_reference/C/txnid.html
---
## DB_TXN-\>id()

``` c
#include <db.h>

u_int32_t
DB_TXN->id(DB_TXN *tid);  
```

The `DB_TXN->id()` method returns the unique transaction id associated with the specified transaction. Locking calls made on behalf of this transaction should use the value returned from `DB_TXN->id()` as the locker parameter to the <a href="lockget.md" class="xref" title="DB_ENV-&gt;lock_get()">DB_ENV-&gt;lock_get()</a> or <a href="lockvec.md" class="xref" title="DB_ENV-&gt;lock_vec()">DB_ENV-&gt;lock_vec()</a> calls.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a>

### See Also

<a href="txn.md#txnlist" class="xref" title="Transaction Subsystem and Related Methods">Transaction Subsystem and Related Methods</a>
