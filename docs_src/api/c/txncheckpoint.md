---
title: "DB_ENV->txn_checkpoint()"
api-name: "DB_ENV->txn_checkpoint()"
source: docs/api_reference/C/txncheckpoint.html
---
## DB_ENV-\>txn_checkpoint()

``` c
#include <db.h>

int
DB_ENV->txn_checkpoint(const DB_ENV *env,
    u_int32_t kbyte, u_int32_t min, u_int32_t flags);  
```

If there has been any logging activity in the database environment since the last checkpoint, the `DB_ENV->txn_checkpoint()` method flushes the underlying memory pool, writes a checkpoint record to the log, and then flushes the log.

The `DB_ENV->txn_checkpoint()` method returns a non-zero error value on failure and 0 on success.

The `DB_ENV->txn_checkpoint()` method is the underlying method used by the <a href="db_checkpoint.md" class="link" title="db_checkpoint">db_checkpoint</a> utility. See the <a href="db_checkpoint.md" class="link" title="db_checkpoint">db_checkpoint</a> utility source code for an example of using `DB_ENV->txn_checkpoint()` in a IEEE/ANSI Std 1003.1 (POSIX) environment.

### Parameters

#### kbyte

If the **kbyte** parameter is non-zero, a checkpoint will be done if more than **kbyte** kilobytes of log data have been written since the last checkpoint.

#### min

If the **min** parameter is non-zero, a checkpoint will be done if more than **min** minutes have passed since the last checkpoint.

#### flags

The **flags** parameter must be set to 0 or the following value:

- `DB_FORCE`

  Force a checkpoint record, even if there has been no activity since the last checkpoint.

### Errors

The `DB_ENV->txn_checkpoint()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a>

### See Also

<a href="txn.md#txnlist" class="xref" title="Transaction Subsystem and Related Methods">Transaction Subsystem and Related Methods</a>
