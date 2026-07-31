---
title: "DB_ENV->get_metadata_dir()"
api-name: "DB_ENV->get_metadata_dir()"
source: docs/api_reference/C/envget_metadata_dir.html
---
## DB_ENV-\>get_metadata_dir()

``` c
#include <db.h>

int
DB_ENV->get_metadata_dir(DB_ENV *envp, const char **dirp); 
```

The `DB_ENV->get_metadata_dir()` method returns the directory where persistent metadata is stored. This location can be set using the <a href="envset_metadata_dir.md" class="xref" title="DB_ENV-&gt;set_metadata_dir()">DB_ENV-&gt;set_metadata_dir()</a> method.

The `DB_ENV->get_metadata_dir()` directory may be called at any time during the life of the application.

The `DB_ENV->get_metadata_dir()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### dirp

The **dirp** parameter references memory into which is copied the directory which contains persistent metadata files.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>, <a href="envset_metadata_dir.md" class="xref" title="DB_ENV-&gt;set_metadata_dir()">DB_ENV-&gt;set_metadata_dir()</a>
