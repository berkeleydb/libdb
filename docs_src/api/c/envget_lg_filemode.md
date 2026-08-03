---
title: "DB_ENV->get_lg_filemode()"
api-name: "DB_ENV->get_lg_filemode()"
source: docs/api_reference/C/envget_lg_filemode.html
---
## DB_ENV-\>get_lg_filemode()

``` c
#include <db.h>

int
DB_ENV->get_lg_filemode(DB_ENV *dbenv, int *lg_modep);  
```

The `DB_ENV->set_lg_filemode()` method returns the log file mode. You can manage this value using the <a href="envset_lg_filemode.md" class="xref" title="DB_ENV-&gt;set_lg_filemode()">DB_ENV-&gt;set_lg_filemode()</a> method.

The `DB_ENV->set_lg_filemode()` method may be called at any time during the life of the application.

The `DB_ENV->set_lg_filemode()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### lg_modep

The `DB_ENV->set_lg_filemode()` method returns the log file mode in **lg_modep**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="logc.md" class="link" title="The DB_LOGC Handle">DB_LOGC</a>, <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a>

### See Also

<a href="lsn.md#loglist" class="xref" title="Logging Subsystem and Related Methods">Logging Subsystem and Related Methods</a>, <a href="envset_lg_filemode.md" class="xref" title="DB_ENV-&gt;set_lg_filemode()">DB_ENV-&gt;set_lg_filemode()</a>
