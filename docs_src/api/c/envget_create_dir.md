---
title: "DB_ENV->get_create_dir()"
api-name: "DB_ENV->get_create_dir()"
source: docs/api_reference/C/envget_create_dir.html
---
## DB_ENV-\>get_create_dir()

``` c
#include <db.h>

int
DB_ENV->get_create_dir(DB_ENV *dbenv, const char **dirp);  
```

The `DB_ENV->get_create_dir()` method returns a pointer to the name of the directory to create databases in.

The `DB_ENV->get_create_dir()` method may be called at any time during the life of the application.

The `DB_ENV->get_create_dir()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### dirp

The `DB_ENV->get_create_dir()` method returns a ponter to the name of the directory in **dirp**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
