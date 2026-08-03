---
title: "DB_ENV->stat_print()"
api-name: "DB_ENV->stat_print()"
source: docs/api_reference/C/envstat.html
---
## DB_ENV-\>stat_print()

``` c
#include <db.h>

int
DB_ENV->stat_print(DB_ENV *dbenv, u_int32_t flags);  
```

The `DB_ENV->stat_print()` method displays the default statistical information. The information is printed to a specified output channel (see the <a href="envset_msgfile.md" class="xref" title="DB_ENV-&gt;set_msgfile()">DB_ENV-&gt;set_msgfile()</a> method for more information), or passed to an application callback function (see the <a href="envset_msgcall.md" class="xref" title="DB_ENV-&gt;set_msgcall()">DB_ENV-&gt;set_msgcall()</a> method for more information).

The `DB_ENV->stat_print()` method may not be called before the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called.

The `DB_ENV->stat_print()` method returns a non-zero error value on failure and 0 on success.

For Berkeley DB SQL environment statistics, see <a href="dbsql.md#dbsql_command_feature" class="xref" title="Command Line Features Unique to dbsql">Command Line Features Unique to dbsql</a>.

### Parameters

#### flags

The **flags** parameter must be set to 0 or by bitwise inclusively **OR**'ing together one or more of the following values:

- `DB_STAT_ALL`

  Display all available information.

- `DB_STAT_CLEAR`

  Reset statistics after displaying their values.

- `DB_STAT_SUBSYSTEM`

  Display information for all configured subsystems.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
