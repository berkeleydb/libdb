---
title: "DB_TXN->set_name()"
api-name: "DB_TXN->set_name()"
source: docs/api_reference/C/txnset_name.html
---
## DB_TXN-\>set_name()

``` c
#include <db.h>

int
DB_TXN->set_name(DB_TXN *txn, const char *name);  
```

The `DB_TXN->set_name()` method associates the specified string with the transaction. The string is returned by <a href="txnstat.md" class="xref" title="DB_ENV-&gt;txn_stat()">DB_ENV-&gt;txn_stat()</a> and displayed by <a href="txnstat_print.md" class="xref" title="DB_ENV-&gt;txn_stat_print()">DB_ENV-&gt;txn_stat_print()</a>.

If the database environment has been configured for logging and the Berkeley DB library was configured with <a href="../../guides/installation/build_unix_conf.md" class="olink">--enable-diagnostic</a>, a debugging log record is written including the transaction ID and the name.

The `DB_TXN->set_name()` method may be called at any time during the life of the application.

The `DB_TXN->set_name()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### name

The **name** parameter is the string to associate with the transaction.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a>

### See Also

<a href="txn.md#txnlist" class="xref" title="Transaction Subsystem and Related Methods">Transaction Subsystem and Related Methods</a>
