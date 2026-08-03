---
title: "DB_TXN->get_name()"
api-name: "DB_TXN->get_name()"
source: docs/api_reference/C/txnget_name.html
---
## DB_TXN-\>get_name()

``` c
#include <db.h>

int
DB_TXN->get_name(DB_TXN *txn, const char **namep);  
```

The `DB_TXN->get_name()` method returns the string associated with the transaction.

The `DB_TXN->get_name()` method may be called at any time during the life of the application.

The `DB_TXN->get_name()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### namep

The `DB_TXN->get_name()` method returns a reference to the string associated with the transaction in **namep**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a>

### See Also

<a href="txn.md#txnlist" class="xref" title="Transaction Subsystem and Related Methods">Transaction Subsystem and Related Methods</a>
