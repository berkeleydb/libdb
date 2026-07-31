---
title: "DB_MULTIPLE_WRITE_INIT"
api-name: "DB_MULTIPLE_WRITE_INIT"
source: docs/api_reference/C/DB_MULTIPLE_WRITE_INIT.html
---
## DB_MULTIPLE_WRITE_INIT

``` c
#include <db.h>

DB_MULTIPLE_WRITE_INIT(void *pointer, DBT *data); 
```

Initialize a DBT containing a bulk buffer for use with the <a href="dbput.md#put_DB_MULTIPLE" class="link">DB_MULTIPLE</a> or <a href="dbput.md#put_DB_MULTIPLE_KEY" class="link">DB_MULTIPLE_KEY</a> flags to the <a href="dbput.md" class="xref" title="DB-&gt;put()">DB-&gt;put()</a> or <a href="dbdel.md" class="xref" title="DB-&gt;del()">DB-&gt;del()</a> methods.

This macro initializes an opaque pointer variable used for adding records to a bulk buffer. Use this macro for buffers that will contain either a data item per record (for use with <a href="dbput.md#put_DB_MULTIPLE" class="link">DB_MULTIPLE</a>), or key/data pairs, where the key is not a record number. For record number keys, use <a href="DB_MULTIPLE_RECNO_WRITE_INIT.md" class="link" title="DB_MULTIPLE_RECNO_WRITE_INIT">DB_MULTIPLE_RECNO_WRITE_INIT</a>.

### Parameters

#### pointer

The **pointer** parameter is an opaque pointer variable to be initialized.

#### data

The **data** parameter is a <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> structure that has been initialized by the application with a buffer to hold multiple records. The **ulen** field must be set to the size of the buffer allocated by the application, and must be a multiple of 4.

### Class

<a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a>

### See Also

<a href="dbt.md#dbtlist" class="xref" title="DBT and Bulk Operations">DBT and Bulk Operations</a>
