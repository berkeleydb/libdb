---
title: "DB_HEAP_RID"
api-name: "DB_HEAP_RID"
source: docs/api_reference/C/db_heap_rid.html
---
## DB_HEAP_RID

``` c
#include <db.h>

struct __db_heap_rid {
    db_pgno_t pgno;         /* Page number. */
    db_indx_t indx;         /* Index in the offset table. */
};  
```

Content used for the key in a Heap database record. Berkeley DB creates this structure for you when you create a record in a Heap database. You should never create this structure or modify the contents of this structure yourself; Berkeley DB must create and manage it for you.

This structure is returned in the **key** DBT parameter of the method that you use to add a record to the Heap database.

### Parameters

#### pgno

The database page number where the record is stored.

#### indx

Index in the offset table where the record can be found.

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>,
