---
title: "DB->sort_multiple()"
api-name: "DB->sort_multiple()"
source: docs/api_reference/C/dbsort_multiple.html
---
## DB-\>sort_multiple()

``` c
#include <db.h>

int
DB->sort_multiple(DB *db, DBT *key, DBT *data, u_int32_t flags); 
```

The `DB->sort_multiple()` method is used to sort a set of <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a>s into database insert order.

If specified the application specific btree comparison and duplicate comparison functions will be used if they are configured.

The key and data parameters must contain pairs of items. That is the n-th entry in **key** must correspond to the n-th entry in **data**.

The `DB->sort_multiple()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### key

The **key** parameter must contain a set of <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> entries in <a href="dbcget.md#dbcget_DB_MULTIPLE" class="link">DB_MULTIPLE</a> or <a href="dbcget.md#dbcget_DB_MULTIPLE_KEY" class="link">DB_MULTIPLE_KEY</a> format.

The sorted entries will be returned in the **key** parameter.

#### data

If non-NULL, the **data** parameter must contain a set of <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a>s entries in <a href="dbcget.md#dbcget_DB_MULTIPLE" class="link">DB_MULTIPLE</a> format. Each entry must correspond to an entry in the **key** parameter.

#### flags

The **flags** parameter must be set to one of the following values:

- DB_MULTIPLE

  Sorts one or two <a href="dbcget.md#dbcget_DB_MULTIPLE" class="link">DB_MULTIPLE</a> format <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a>s. Assumes that **key** and **data** specify pairs of key and data items to sort together. If the **data** parameter is NULL the API will sort the key arrays according to the btree comparison function.

- DB_MULTIPLE_KEY

  Sorts a <a href="dbcget.md#dbcget_DB_MULTIPLE_KEY" class="link">DB_MULTIPLE_KEY</a> format <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a>.

### Errors

The `DB->sort_multiple()` method may fail and return one of the following non-zero errors:

#### EACCES

An attempt was made to modify a read-only database.

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>

<a href="dbt.md#dbtlist" class="xref" title="DBT and Bulk Operations">DBT and Bulk Operations</a>
