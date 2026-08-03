---
title: "DB->get_partition_keys()"
api-name: "DB->get_partition_keys()"
source: docs/api_reference/C/dbget_partition_keys.html
---
## DB-\>get_partition_keys()

``` c
#include <db.h>

int
DB->get_partition_keys(DB *db, u_int32_t *partsp, DBT *keysp);  
```

The `DB->get_partition_keys()` method returns the partitioning information as set by the <a href="dbset_partition.md" class="xref" title="DB-&gt;set_partition()">DB-&gt;set_partition()</a> method.

The `DB->get_partition_keys()` method may be called at any time during the life of the application.

The `DB->get_partition_keys()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### partsp

The `DB->get_partition_keys()` method returns number of partitions in the **partsp** parameter.

#### keysp

The **keysp** parameter will be set to the array of partitioning keys.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>, <a href="dbset_partition.md" class="xref" title="DB-&gt;set_partition()">DB-&gt;set_partition()</a>
