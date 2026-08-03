---
title: "DB_ENV->log_put()"
api-name: "DB_ENV->log_put()"
source: docs/api_reference/C/logput.html
---
## DB_ENV-\>log_put()

``` c
#include <db.h>

int
DB_ENV->log_put(DB_ENV *env,
    DB_LSN *lsn, const DBT *data, u_int32_t flags);  
```

The `DB_ENV->log_put()` method appends records to the log. The <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a> of the put record is returned in the **lsn** parameter.

The `DB_ENV->log_put()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### lsn

The **lsn** parameter references memory into which the <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a> of the put record is copied.

#### data

The **data** parameter is the record to write to the log.

The caller is responsible for providing any necessary structure to **data**. (For example, in a write-ahead logging protocol, the application must understand what part of **data** is an operation code, what part is redo information, and what part is undo information. In addition, most transaction managers will store in **data** the <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a> of the previous log record for the same transaction, to support chaining back through the transaction's log records during undo.)

#### flags

The **flags** parameter must be set to 0 or the following value:

- `DB_FLUSH`

  The log is forced to disk after this record is written, guaranteeing that all records with <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a> values less than or equal to the one being "put" are on disk before `DB_ENV->log_put()` returns.

### Errors

The `DB_ENV->log_put()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the record to be logged is larger than the maximum log record; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="logc.md" class="link" title="The DB_LOGC Handle">DB_LOGC</a>, <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a>

### See Also

<a href="lsn.md#loglist" class="xref" title="Logging Subsystem and Related Methods">Logging Subsystem and Related Methods</a>
