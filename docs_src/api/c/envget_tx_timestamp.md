---
title: "DB_ENV->get_tx_timestamp()"
api-name: "DB_ENV->get_tx_timestamp()"
source: docs/api_reference/C/envget_tx_timestamp.html
---
## DB_ENV-\>get_tx_timestamp()

``` c
#include <db.h>

int
DB_ENV->get_tx_timestamp(DB_ENV *dbenv, time_t *timestampp);  
```

The `DB_ENV->get_tx_timestamp()` method returns the recovery timestamp. This value can be modified using the <a href="envset_tx_timestamp.md" class="xref" title="DB_ENV-&gt;set_tx_timestamp()">DB_ENV-&gt;set_tx_timestamp()</a> method.

The `DB_ENV->get_tx_timestamp()` method may be called at any time during the life of the application.

The `DB_ENV->get_tx_timestamp()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### timestampp

The `DB_ENV->get_tx_timestamp()` method returns the recovery timestamp in **timestampp**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a>

### See Also

<a href="txn.md#txnlist" class="xref" title="Transaction Subsystem and Related Methods">Transaction Subsystem and Related Methods</a>, <a href="envset_tx_timestamp.md" class="xref" title="DB_ENV-&gt;set_tx_timestamp()">DB_ENV-&gt;set_tx_timestamp()</a>
