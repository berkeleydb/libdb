---
title: "DB_ENV->get_lg_bsize()"
api-name: "DB_ENV->get_lg_bsize()"
source: docs/api_reference/C/envget_lg_bsize.html
---
## DB_ENV-\>get_lg_bsize()

``` c
#include <db.h>

int
DB_ENV->get_lg_bsize(DB_ENV *dbenv, u_int32_t *lg_bsizep);  
```

The `DB_ENV->get_lg_bsize()` method returns the size of the log buffer, in bytes. You can manage this value using the <a href="envset_lg_bsize.md" class="xref" title="DB_ENV-&gt;set_lg_bsize()">DB_ENV-&gt;set_lg_bsize()</a> method.

The `DB_ENV->get_lg_bsize()` method may be called at any time during the life of the application.

The `DB_ENV->get_lg_bsize()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### lg_bsizep

The `DB_ENV->get_lg_bsize()` method returns the size of the log buffer, in bytes in **lg_bsizep**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="logc.md" class="link" title="The DB_LOGC Handle">DB_LOGC</a>, <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a>

### See Also

<a href="lsn.md#loglist" class="xref" title="Logging Subsystem and Related Methods">Logging Subsystem and Related Methods</a>, <a href="envset_lg_bsize.md" class="xref" title="DB_ENV-&gt;set_lg_bsize()">DB_ENV-&gt;set_lg_bsize()</a>
