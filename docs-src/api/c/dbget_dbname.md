---
title: "DB->get_dbname()"
api-name: "DB->get_dbname()"
source: docs/api_reference/C/dbget_dbname.html
---
## DB-\>get_dbname()

``` c
#include <db.h>

int
DB->get_dbname(DB *db, const char **filenamep, const char **dbnamep);  
```

The `DB->get_dbname()` method returns the filename and database name used by the DB handle.

The `DB->get_dbname()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### filenamep

The **filenamep** parameter references memory into which a pointer to the current filename is copied.

#### dbnamep

The **dbnamep** parameter references memory into which a pointer to the current database name is copied.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
