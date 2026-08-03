---
title: "DB_ENV->get_tmp_dir()"
api-name: "DB_ENV->get_tmp_dir()"
source: docs/api_reference/C/envget_tmp_dir.html
---
## DB_ENV-\>get_tmp_dir()

``` c
#include <db.h>

int
DB_ENV->get_tmp_dir(DB_ENV *dbenv, const char **dirp);  
```

The `DB_ENV->get_tmp_dir()` method returns the database environment temporary file directory.

The `DB_ENV->get_tmp_dir()` method may be called at any time during the life of the application.

The `DB_ENV->get_tmp_dir()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### dirp

The `DB_ENV->get_tmp_dir()` method returns a reference to the database environment temporary file directory in **dirp**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>, <a href="envset_tmp_dir.md" class="xref" title="DB_ENV-&gt;set_tmp_dir()">DB_ENV-&gt;set_tmp_dir()</a>
