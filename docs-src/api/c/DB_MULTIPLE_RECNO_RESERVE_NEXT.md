---
title: "DB_MULTIPLE_RECNO_RESERVE_NEXT"
api-name: "DB_MULTIPLE_RECNO_RESERVE_NEXT"
source: docs/api_reference/C/DB_MULTIPLE_RECNO_RESERVE_NEXT.html
---
## DB_MULTIPLE_RECNO_RESERVE_NEXT

``` c
#include <db.h>

DB_MULTIPLE_RECNO_RESERVE_NEXT(void *pointer, DBT *dbt, db_recno_t recno, 
    void *ddest, size_t dlen); 
```

Reserves space for a record number / data pair in a bulk buffer.

### Parameters

#### pointer

The **pointer** parameter is a variable that must have been initialized by a call to <a href="DB_MULTIPLE_RECNO_WRITE_INIT.md" class="xref" title="DB_MULTIPLE_RECNO_WRITE_INIT">DB_MULTIPLE_RECNO_WRITE_INIT</a>.

#### dbt

The **dbt** parameter is a <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> structure initialized with <a href="DB_MULTIPLE_RECNO_WRITE_INIT.md" class="xref" title="DB_MULTIPLE_RECNO_WRITE_INIT">DB_MULTIPLE_RECNO_WRITE_INIT</a>.

#### recno

The record number to be copied into the bulk buffer.

This parameter is set to 0 if the data item does not fit in the buffer.

#### ddest

The **ddest** parameter is set to the location reserved in the bulk buffer for the data item.

This parameter is set to NULL if the data item does not fit in the buffer.

#### dlen

The number of bytes to be reserved.

### Class

<a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a>

### See Also

<a href="dbt.md#dbtlist" class="xref" title="DBT and Bulk Operations">DBT and Bulk Operations</a>
