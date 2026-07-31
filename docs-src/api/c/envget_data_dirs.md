---
title: "DB_ENV->get_data_dirs()"
api-name: "DB_ENV->get_data_dirs()"
source: docs/api_reference/C/envget_data_dirs.html
---
## DB_ENV-\>get_data_dirs()

``` c
#include <db.h>

int
DB_ENV->get_data_dirs(DB_ENV *dbenv, const char ***dirpp);  
```

The `DB_ENV->get_data_dirs()` method returns the NULL-terminated array of directories.

The `DB_ENV->get_data_dirs()` method may be called at any time during the life of the application.

The `DB_ENV->get_data_dirs()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### dirpp

The `DB_ENV->get_data_dirs()` method returns a reference to the NULL-terminated array of directories in **dirpp**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
