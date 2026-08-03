---
title: "DB_ENV->log_printf()"
api-name: "DB_ENV->log_printf()"
source: docs/api_reference/C/logprintf.html
---
## DB_ENV-\>log_printf()

``` c
#include <db.h>

int
DB_ENV->log_printf(DB_ENV *env, DB_TXN *txnid, const char *fmt, ...);  
```

The `DB_ENV->log_printf()` method appends an informational message to the Berkeley DB database environment log files.

The `DB_ENV->log_printf()` method allows applications to include information in the database environment log files, for later review using the <a href="db_printlog.md" class="link" title="db_printlog">db_printlog</a> utility. This method is intended for debugging and performance tuning.

The `DB_ENV->log_printf()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### txnid

If the logged message refers to an application-specified transaction, the **txnid** parameter is a transaction handle returned from <a href="txnbegin.md" class="xref" title="DB_ENV-&gt;txn_begin()">DB_ENV-&gt;txn_begin()</a>; otherwise NULL.

#### fmt

A format string that specifies how subsequent arguments (or arguments accessed via the variable-length argument facilities of stdarg(3)) are converted for output. The format string may contain any formatting directives supported by the underlying C library vsnprintf(3) function.

### Errors

The `DB_ENV->log_printf()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="logc.md" class="link" title="The DB_LOGC Handle">DB_LOGC</a>, <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a>

### See Also

<a href="lsn.md#loglist" class="xref" title="Logging Subsystem and Related Methods">Logging Subsystem and Related Methods</a>
