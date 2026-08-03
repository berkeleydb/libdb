---
title: "DB_ENV->set_tx_timestamp()"
api-name: "DB_ENV->set_tx_timestamp()"
source: docs/api_reference/C/envset_tx_timestamp.html
---
## DB_ENV-\>set_tx_timestamp()

``` c
#include <db.h>

int
DB_ENV->set_tx_timestamp(DB_ENV *dbenv, time_t *timestamp);  
```

Recover to the time specified by **timestamp** rather than to the most current possible date.

Once a database environment has been upgraded to a new version of Berkeley DB involving a log format change (see <a href="../../guides/upgrading/upgrade_process.md" class="olink">Upgrading Berkeley DB installations</a>), it is no longer possible to recover to a specific time before that upgrade.

The `DB_ENV->set_tx_timestamp()` method configures operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle, not all operations performed on the underlying database environment.

The `DB_ENV->set_tx_timestamp()` method may not be called after the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called.

The `DB_ENV->set_tx_timestamp()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### timestamp

The **timestamp** parameter references the memory location where the recovery timestamp is located.

The **timestamp** parameter should be the number of seconds since 0 hours, 0 minutes, 0 seconds, January 1, 1970, Coordinated Universal Time; that is, the Epoch.

### Errors

The `DB_ENV->set_tx_timestamp()` method may fail and return one of the following non-zero errors:

#### EINVAL

If it is not possible to recover to the specified time using the log files currently present in the environment; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a>

### See Also

<a href="txn.md#txnlist" class="xref" title="Transaction Subsystem and Related Methods">Transaction Subsystem and Related Methods</a>
