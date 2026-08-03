---
title: "DB_ENV->get_intermediate_dir_mode()"
api-name: "DB_ENV->get_intermediate_dir_mode()"
source: docs/api_reference/C/envget_intermediate_dir_mode.html
---
## DB_ENV-\>get_intermediate_dir_mode()

``` c
#include <db.h>

int
DB_ENV->get_intermediate_dir_mode(DB_ENV *dbenv, const char **modep);  
```

The `DB_ENV->get_intermediate_dir_mode()` method returns the intermediate directory permissions.

Intermediate directories are directories needed for recovery. Normally, Berkeley DB does not create these directories and will do so only if the <a href="envset_intermediate_dir_mode.md" class="xref" title="DB_ENV-&gt;set_intermediate_dir_mode()">DB_ENV-&gt;set_intermediate_dir_mode()</a> method is called.

The `DB_ENV->get_intermediate_dir_mode()` method may be called at any time during the life of the application.

The `DB_ENV->get_intermediate_dir_mode()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### modep

The `DB_ENV->get_intermediate_dir_mode()` method returns a reference to the intermediate directory permissions in **modep**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
