---
title: "DB->get_env()"
api-name: "DB->get_env()"
source: docs/api_reference/C/dbgetenv.html
---
## DB-\>get_env()

``` c
#include <db.h>

DB_ENV *
DB->get_env(DB *db);  
```

The `DB->get_env()` method returns the handle for the database environment underlying the database.

The `DB->get_env()` method may be called at any time during the life of the application.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
