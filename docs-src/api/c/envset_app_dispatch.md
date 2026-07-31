---
title: "DB_ENV->set_app_dispatch()"
api-name: "DB_ENV->set_app_dispatch()"
source: docs/api_reference/C/envset_app_dispatch.html
---
## DB_ENV-\>set_app_dispatch()

``` c
#include <db.h>

int
DB_ENV->set_app_dispatch(DB_ENV *dbenv,
    int (*tx_recover)(DB_ENV *dbenv,
    DBT *log_rec, DB_LSN *lsn, db_recops op));  
```

Declare a function to be called during transaction abort and recovery to process application-specific log records.

The `DB_ENV->set_app_dispatch()` method configures operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle, not all operations performed on the underlying database environment.

The `DB_ENV->set_app_dispatch()` method may not be called after the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called. If the database environment already exists when <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> is called, the information specified to `DB_ENV->set_app_dispatch()` must be consistent with the existing environment or corruption can occur.

The `DB_ENV->set_app_dispatch()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### tx_recover

The **tx_recover** parameter is the application's abort and recovery function. The function takes four parameters:

- `dbenv`

  The **dbenv** parameter is the enclosing database environment handle.

- `log_rec`

  The **log_rec** parameter is a log record.

- `lsn`

  The **lsn** parameter is a log sequence number.

- `op`

  The **op** parameter is one of the following values:

  - `DB_TXN_BACKWARD_ROLL`

    The log is being read backward to determine which transactions have been committed and to abort those operations that were not; undo the operation described by the log record.

  - `DB_TXN_FORWARD_ROLL`

    The log is being played forward; redo the operation described by the log record.

  - `DB_TXN_ABORT`

    The log is being read backward during a transaction abort; undo the operation described by the log record.

  - `DB_TXN_APPLY`

    The log is being applied on a replica site; redo the operation described by the log record.

  - `DB_TXN_PRINT`

    The log is being printed for debugging purposes; print the contents of this log record in the desired format.

  The DB_TXN_FORWARD_ROLL and DB_TXN_APPLY operations frequently imply the same actions, redoing changes that appear in the log record, although if a recovery function is to be used on a replication client where reads may be taking place concurrently with the processing of incoming messages, DB_TXN_APPLY operations should also perform appropriate locking. The macro DB_REDO(op) checks that the operation is one of DB_TXN_FORWARD_ROLL or DB_TXN_APPLY, and should be used in the recovery code to refer to the conditions under which operations should be redone. Similarly, the macro DB_UNDO(op) checks if the operation is one of DB_TXN_BACKWARD_ROLL or DB_TXN_ABORT.

The function must return 0 on success and either **errno** or a value outside of the Berkeley DB error name space on failure.

### Errors

The `DB_ENV->set_app_dispatch()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="txn.md" class="link" title="Chapter 12.  The DB_TXN Handle">DB_TXN</a>

### See Also

<a href="txn.md#txnlist" class="xref" title="Transaction Subsystem and Related Methods">Transaction Subsystem and Related Methods</a>
