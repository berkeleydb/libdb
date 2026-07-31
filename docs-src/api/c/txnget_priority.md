---
title: "DB_TXN->get_priority()"
api-name: "DB_TXN->get_priority()"
source: docs/api_reference/C/txnget_priority.html
---
## DB_TXN-\>get_priority()

``` c
#include <db.h>

int
DB_TXN->get_priority(DB_TXN *tid, u_int32_t *priority);  
```

The `DB_TXN->get_priority()` method gets the priority value of the specified transaction.

The `DB_TXN->get_priority()` method may be called at any time during the life of the transaction.

The `DB_TXN->get_priority()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### priority

Upon return, the **priority** parameter will point to a value between 0 and 2^32-1.

### Errors

The `DB_TXN->get_priority()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a>

### See Also

<a href="txn.md#txnlist" class="xref" title="Transaction Subsystem and Related Methods">Transaction Subsystem and Related Methods</a>
