---
title: "DB_ENV->log_stat_print()"
api-name: "DB_ENV->log_stat_print()"
source: docs/api_reference/C/logstat_print.html
---
## DB_ENV-\>log_stat_print()

``` c
#include <db.h>

int
DB_ENV->log_stat_print(DB_ENV *env, u_int32_t flags);  
```

The `DB_ENV->log_stat_print()` method displays the logging subsystem statistical information, as described for the `DB_ENV->log_stat()` method. The information is printed to a specified output channel (see the <a href="envset_msgfile.md" class="xref" title="DB_ENV-&gt;set_msgfile()">DB_ENV-&gt;set_msgfile()</a> method for more information), or passed to an application callback function (see the <a href="envset_msgcall.md" class="xref" title="DB_ENV-&gt;set_msgcall()">DB_ENV-&gt;set_msgcall()</a> method for more information).

The `DB_ENV->log_stat_print()` method may not be called before the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called.

The `DB_ENV->log_stat_print()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flags

The **flags** parameter must be set to 0 or by bitwise inclusively **OR**'ing together one or more of the following values:

- `DB_STAT_ALL`

  Display all available information.

- `DB_STAT_CLEAR`

  Reset statistics after displaying their values.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="logc.md" class="link" title="The DB_LOGC Handle">DB_LOGC</a>, <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a>

### See Also

<a href="lsn.md#loglist" class="xref" title="Logging Subsystem and Related Methods">Logging Subsystem and Related Methods</a>
