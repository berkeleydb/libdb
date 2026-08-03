---
title: "DB_MULTIPLE_KEY_NEXT"
api-name: "DB_MULTIPLE_KEY_NEXT"
source: docs/api_reference/C/DB_MULTIPLE_KEY_NEXT.html
---
## DB_MULTIPLE_KEY_NEXT

``` c
#include <db.h>

DB_MULTIPLE_KEY_NEXT(void *pointer, DBT *data,
    void *retkey, size_t retklen, void *retdata, size_t retdlen); 
```

If either of the <a href="dbcget.md#dbcget_DB_MULTIPLE" class="link">DB_MULTIPLE</a> or <a href="dbcget.md#dbcget_DB_MULTIPLE_KEY" class="link">DB_MULTIPLE_KEY</a> flags were specified to the <a href="dbget.md" class="xref" title="DB-&gt;get()">DB-&gt;get()</a> or <a href="dbcget.md" class="xref" title="DBcursor-&gt;get()">DBcursor-&gt;get()</a> methods, the data <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> returned by those interfaces will refer to a buffer that is filled with data. Access to that data is through the DB_MULTIPLE\_\* macros.

Returns the next <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> in the bulk retrieval set. Use this macro with <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> structures obtained from a database that uses the Btree or Hash access methods.

### Parameters

#### pointer

The **pointer** parameter is a variable that must have been initialized by a call to <a href="DB_MULTIPLE_INIT.md" class="xref" title="DB_MULTIPLE_INIT">DB_MULTIPLE_INIT</a>.

This parameter is set to NULL if there are no more key/data pairs in the returned set.

#### data

The **data** parameter is a <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> structure returned from a successful call to <a href="dbcget.md" class="xref" title="DBcursor-&gt;get()">DBcursor-&gt;get()</a> with the Btree or Hash access methods for which the <a href="dbcget.md#dbcget_DB_MULTIPLE_KEY" class="link">DB_MULTIPLE_KEY</a> flag was specified.

The **data** parameter must have been initialized by a call to <a href="DB_MULTIPLE_INIT.md" class="xref" title="DB_MULTIPLE_INIT">DB_MULTIPLE_INIT</a>.

#### retkey

The **retkey** parameter is set to the next key element in the returned set.

#### retklen

The **retklen** parameter is set to the length, in bytes, of the next key element.

#### retdata

The **retdata** parameter is set to the next data element in the returned set.

#### retdlen

The **retdlen** parameter is set to the length, in bytes, of the next data element.

### Class

<a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a>

### See Also

<a href="dbt.md#dbtlist" class="xref" title="DBT and Bulk Operations">DBT and Bulk Operations</a>
