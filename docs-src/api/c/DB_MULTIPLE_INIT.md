---
title: "DB_MULTIPLE_INIT"
api-name: "DB_MULTIPLE_INIT"
source: docs/api_reference/C/DB_MULTIPLE_INIT.html
---
## DB_MULTIPLE_INIT

``` c
#include <db.h>

DB_MULTIPLE_INIT(void *pointer, DBT *data); 
```

If either of the <a href="dbcget.md#dbcget_DB_MULTIPLE" class="link">DB_MULTIPLE</a> or <a href="dbcget.md#dbcget_DB_MULTIPLE_KEY" class="link">DB_MULTIPLE_KEY</a> flags were specified to the <a href="dbget.md" class="xref" title="DB-&gt;get()">DB-&gt;get()</a> or <a href="dbcget.md" class="xref" title="DBcursor-&gt;get()">DBcursor-&gt;get()</a> methods, the data <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> returned by those interfaces will refer to a buffer that is filled with data. Access to that data is through the DB_MULTIPLE\_\* macros.

This macro initializes a variable used for bulk retrieval.

### Parameters

#### pointer

The **pointer** parameter is a variable to be initialized.

#### data

The **data** parameter is a <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> structure returned from a successful call to <a href="dbget.md" class="xref" title="DB-&gt;get()">DB-&gt;get()</a> or <a href="dbcget.md" class="xref" title="DBcursor-&gt;get()">DBcursor-&gt;get()</a> for which one of the <a href="dbcget.md#dbcget_DB_MULTIPLE" class="link">DB_MULTIPLE</a> or <a href="dbcget.md#dbcget_DB_MULTIPLE_KEY" class="link">DB_MULTIPLE_KEY</a> flags were specified.

### Class

<a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a>

### See Also

<a href="dbt.md#dbtlist" class="xref" title="DBT and Bulk Operations">DBT and Bulk Operations</a>
