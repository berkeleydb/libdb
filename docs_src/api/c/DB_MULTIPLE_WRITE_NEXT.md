---
title: "DB_MULTIPLE_WRITE_NEXT"
api-name: "DB_MULTIPLE_WRITE_NEXT"
source: docs/api_reference/C/DB_MULTIPLE_WRITE_NEXT.html
---
## DB_MULTIPLE_WRITE_NEXT

``` c
#include <db.h>

DB_MULTIPLE_WRITE_NEXT(void *pointer, DBT *dbt, void *data, 
    size_t dlen); 
```

Appends a data item to the bulk buffer.

### Parameters

#### pointer

The **pointer** parameter is a variable that must have been initialized by a call to <a href="DB_MULTIPLE_WRITE_INIT.md" class="xref" title="DB_MULTIPLE_WRITE_INIT">DB_MULTIPLE_WRITE_INIT</a>.

This parameter is set to NULL if the data item does not fit in the buffer.

#### dbt

The **dbt** parameter is a <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> structure initialized with <a href="DB_MULTIPLE_WRITE_INIT.md" class="xref" title="DB_MULTIPLE_WRITE_INIT">DB_MULTIPLE_WRITE_INIT</a>.

#### data

A pointer to the bytes to be copied into the bulk buffer.

#### dlen

The number of bytes to be copied.

### Class

<a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a>

### See Also

<a href="dbt.md#dbtlist" class="xref" title="DBT and Bulk Operations">DBT and Bulk Operations</a>
