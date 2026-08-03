---
title: "DB_ENV->get_home()"
api-name: "DB_ENV->get_home()"
source: docs/api_reference/C/envget_home.html
---
## DB_ENV-\>get_home()

``` c
#include <db.h>

int
DB_ENV->get_home(DB_ENV *dbenv, const char **homep);  
```

The `DB_ENV->get_home()` method returns the database environment home directory. This directory is normally identified when the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called.

The `DB_ENV->get_home()` method may be called at any time during the life of the application.

The `DB_ENV->get_home()` method returns a non-zero error value on failure and 0 on success.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
