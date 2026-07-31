---
title: "DB_MPOOLFILE->get_pgcookie()"
api-name: "DB_MPOOLFILE->get_pgcookie()"
source: docs/api_reference/C/mempget_pgcookie.html
---
## DB_MPOOLFILE-\>get_pgcookie()

``` c
#include <db.h>

int
DB_MPOOLFILE->get_pgcookie(DB_MPOOLFILE *mpf, DBT *dbt);  
```

The `DB_MPOOLFILE->get_pgcookie()` method returns the byte string provided to the functions registered to do input or output processing of the file's pages as they are read from or written to, the backing filesystem store. This byte string is configured using the <a href="mempset_pgcookie.md" class="xref" title="DB_MPOOLFILE-&gt;set_pgcookie()">DB_MPOOLFILE-&gt;set_pgcookie()</a> method.

The `DB_MPOOLFILE->get_pgcookie()` method may be called at any time during the life of the application.

The `DB_MPOOLFILE->get_pgcookie()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### dbt

The `DB_MPOOLFILE->get_pgcookie()` method returns a reference to the byte string in **dbt**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>, <a href="mempset_pgcookie.md" class="xref" title="DB_MPOOLFILE-&gt;set_pgcookie()">DB_MPOOLFILE-&gt;set_pgcookie()</a>
