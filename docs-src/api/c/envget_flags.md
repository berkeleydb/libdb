---
title: "DB_ENV->get_flags()"
api-name: "DB_ENV->get_flags()"
source: docs/api_reference/C/envget_flags.html
---
## DB_ENV-\>get_flags()

``` c
#include <db.h>

int
DB_ENV->get_flags(DB_ENV *dbenv, u_int32_t *flagsp);  
```

The `DB_ENV->get_flags()` method returns the configuration flags set for a <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle. These flags are set using the <a href="envset_flags.md" class="xref" title="DB_ENV-&gt;set_flags()">DB_ENV-&gt;set_flags()</a> method.

The `DB_ENV->get_flags()` method may be called at any time during the life of the application.

The `DB_ENV->get_flags()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flagsp

The `DB_ENV->get_flags()` method returns the configuration flags in **flagsp**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
