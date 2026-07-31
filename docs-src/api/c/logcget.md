---
title: "DB_LOGC->get()"
api-name: "DB_LOGC->get()"
source: docs/api_reference/C/logcget.html
---
## DB_LOGC-\>get()

``` c
#include <db.h>

int
DB_LOGC->get(DB_LOGC *logc, DB_LSN *lsn, DBT *data, u_int32_t flags);  
```

The `DB_LOGC->get()` method returns records from the log.

Unless otherwise specified, the `DB_LOGC->get()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### lsn

When the **flag** parameter is set to DB_CURRENT, DB_FIRST, DB_LAST, DB_NEXT or DB_PREV, the **lsn** parameter is overwritten with the <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a> value of the record retrieved. When **flag** is set to DB_SET, the **lsn** parameter is the <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a> value of the record to be retrieved.

#### data

The data field of the **data** structure is set to the record retrieved, and the size field indicates the number of bytes in the record. See <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> for a description of other fields in the **data** structure. The <a href="dbt.md#dbt_DB_DBT_MALLOC" class="link">DB_DBT_MALLOC</a>, <a href="dbt.md#dbt_DB_DBT_REALLOC" class="link">DB_DBT_REALLOC</a> and <a href="dbt.md#dbt_DB_DBT_USERMEM" class="link">DB_DBT_USERMEM</a> flags may be specified for any <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> used for data retrieval.

#### flags

The **flags** parameter must be set to one of the following values:

- `DB_CURRENT`

  Return the log record to which the log currently refers.

- `DB_FIRST`

  The first record from any of the log files found in the log directory is returned in the **data** parameter. The **lsn** parameter is overwritten with the <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a> of the record returned.

  The `DB_LOGC->get()` method will return <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_NOTFOUND" class="olink">DB_NOTFOUND</a> if DB_FIRST is set and the log is empty.

- `DB_LAST`

  The last record in the log is returned in the **data** parameter. The **lsn** parameter is overwritten with the <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a> of the record returned.

  The `DB_LOGC->get()` method will return <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_NOTFOUND" class="olink">DB_NOTFOUND</a> if DB_LAST is set and the log is empty.

- `DB_NEXT`

  The current log position is advanced to the next record in the log, and that record is returned in the **data** parameter. The **lsn** parameter is overwritten with the <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a> of the record returned.

  If the cursor has not been initialized via DB_FIRST, DB_LAST, DB_SET, DB_NEXT, or DB_PREV, `DB_LOGC->get()` will return the first record in the log.

  The `DB_LOGC->get()` method will return <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_NOTFOUND" class="olink">DB_NOTFOUND</a> if DB_NEXT is set and the last log record has already been returned or the log is empty.

- `DB_PREV`

  The current log position is advanced to the previous record in the log, and that record is returned in the **data** parameter. The **lsn** parameter is overwritten with the <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a> of the record returned.

  If the cursor has not been initialized via DB_FIRST, DB_LAST, DB_SET, DB_NEXT, or DB_PREV, `DB_LOGC->get()` will return the last record in the log.

  The `DB_LOGC->get()` method will return <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_NOTFOUND" class="olink">DB_NOTFOUND</a> if DB_PREV is set and the first log record has already been returned or the log is empty.

- `DB_SET`

  Retrieve the record specified by the **lsn** parameter.

### Errors

The `DB_LOGC->get()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the DB_CURRENT flag was set and the log cursor has not yet been initialized; the DB_CURRENT, DB_NEXT, or DB_PREV flags were set and the log was opened with the DB_THREAD flag set; the DB_SET flag was set and the specified log sequence number does not appear in the log; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="logc.md" class="link" title="The DB_LOGC Handle">DB_LOGC</a>, <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a>

### See Also

<a href="lsn.md#loglist" class="xref" title="Logging Subsystem and Related Methods">Logging Subsystem and Related Methods</a>
