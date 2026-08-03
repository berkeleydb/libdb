---
title: "DB_ENV->log_flush()"
api-name: "DB_ENV->log_flush()"
source: docs/api_reference/C/logflush.html
---
## DB_ENV-\>log_flush()

``` c
#include <db.h>

int
DB_ENV->log_flush(DB_ENV *env, const DB_LSN *lsn);  
```

The `DB_ENV->log_flush()` method writes log records to disk.

The `DB_ENV->log_flush()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### lsn

All log records with <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a> values less than or equal to the **lsn** parameter are written to disk. If **lsn** is NULL, all records in the log are flushed.

### Errors

The `DB_ENV->log_flush()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="logc.md" class="link" title="The DB_LOGC Handle">DB_LOGC</a>, <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a>

### See Also

<a href="lsn.md#loglist" class="xref" title="Logging Subsystem and Related Methods">Logging Subsystem and Related Methods</a>
