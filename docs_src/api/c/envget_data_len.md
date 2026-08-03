---
title: "DB_ENV->get_data_len()"
api-name: "DB_ENV->get_data_len()"
source: docs/api_reference/C/envget_data_len.html
---
## DB_ENV-\>get_data_len()

``` c
#include <db.h>

int
DB_ENV->get_data_len(DB_ENV *dbenv, u_int32_t *bytes);  
```

The `DB_ENV->get_data_len()` method returns the maximum number of bytes to display for each key/data item when dumping the database or printing the log. This limit can be set using the <a href="envset_data_len.md" class="xref" title="DB_ENV-&gt;set_data_len()">DB_ENV-&gt;set_data_len()</a> method.

The `DB_ENV->get_data_len()` method may be called at any time during the life of the application.

The `DB_ENV->get_data_len()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### bytes

The `bytes` parameter references memory into which is copied the maximum number of bytes to display when dumping the database or printing the log.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
