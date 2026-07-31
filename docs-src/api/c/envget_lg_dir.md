---
title: "DB_ENV->get_lg_dir()"
api-name: "DB_ENV->get_lg_dir()"
source: docs/api_reference/C/envget_lg_dir.html
---
## DB_ENV-\>get_lg_dir()

``` c
#include <db.h>

int
DB_ENV->get_lg_dir(DB_ENV *dbenv, const char **dirp);  
```

The `DB_ENV->get_lg_dir()` method returns the log directory, which is the location for logging files. You can manage this value using the <a href="envset_lg_dir.md" class="xref" title="DB_ENV-&gt;set_lg_dir()">DB_ENV-&gt;set_lg_dir()</a> method.

The `DB_ENV->get_lg_dir()` method may be called at any time during the life of the application.

The `DB_ENV->get_lg_dir()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### dirp

The `DB_ENV->get_lg_dir()` method returns a reference to the log directory in **dirp**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="logc.md" class="link" title="The DB_LOGC Handle">DB_LOGC</a>, <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a>

### See Also

<a href="lsn.md#loglist" class="xref" title="Logging Subsystem and Related Methods">Logging Subsystem and Related Methods</a>, <a href="envset_lg_dir.md" class="xref" title="DB_ENV-&gt;set_lg_dir()">DB_ENV-&gt;set_lg_dir()</a>
