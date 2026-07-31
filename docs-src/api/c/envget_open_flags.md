---
title: "DB_ENV->get_open_flags()"
api-name: "DB_ENV->get_open_flags()"
source: docs/api_reference/C/envget_open_flags.html
---
## DB_ENV-\>get_open_flags()

``` c
#include <db.h>

int
DB_ENV->get_open_flags(DB_ENV *dbenv, u_int32_t *flagsp);  
```

The `DB_ENV->get_open_flags()` method returns the open method flags originally used to create the database environment.

The `DB_ENV->get_open_flags()` method may not be called before the `DB_ENV->open()` method is called.

The `DB_ENV->get_open_flags()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flagsp

The `DB_ENV->get_open_flags()` method returns the open method flags originally used to create the database environment in **flagsp**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>, <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a>
