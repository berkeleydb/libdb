---
title: "DB_ENV->get_tx_max()"
api-name: "DB_ENV->get_tx_max()"
source: docs/api_reference/C/envget_tx_max.html
---
## DB_ENV-\>get_tx_max()

``` c
#include <db.h>

int
DB_ENV->get_tx_max(DB_ENV *dbenv, u_int32_t *tx_maxp);  
```

The `DB_ENV->get_tx_max()` method returns the maximum number of active transactions currently configured for the environment. You can manage this value using the <a href="envset_tx_max.md" class="xref" title="DB_ENV-&gt;set_tx_max()">DB_ENV-&gt;set_tx_max()</a> method.

The `DB_ENV->get_tx_max()` method may be called at any time during the life of the application.

The `DB_ENV->get_tx_max()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### tx_maxp

The `DB_ENV->get_tx_max()` method returns the number of active transactions in **tx_maxp**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a>

### See Also

<a href="txn.md#txnlist" class="xref" title="Transaction Subsystem and Related Methods">Transaction Subsystem and Related Methods</a>, <a href="envset_tx_max.md" class="xref" title="DB_ENV-&gt;set_tx_max()">DB_ENV-&gt;set_tx_max()</a>
