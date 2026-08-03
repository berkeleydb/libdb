---
title: "DB_ENV->rep_stat_print()"
api-name: "DB_ENV->rep_stat_print()"
source: docs/api_reference/C/repstat_print.html
---
## DB_ENV-\>rep_stat_print()

``` c
#include <db.h>

int
DB_ENV->rep_stat_print(DB_ENV *env, u_int32_t flags);  
```

The `DB_ENV->rep_stat_print()` method displays the replication subsystem statistical information, as described for the `DB_ENV->rep_stat()` method. The information is printed to a specified output channel (see the <a href="envset_msgfile.md" class="xref" title="DB_ENV-&gt;set_msgfile()">DB_ENV-&gt;set_msgfile()</a> method for more information), or passed to an application callback function (see the <a href="envset_msgcall.md" class="xref" title="DB_ENV-&gt;set_msgcall()">DB_ENV-&gt;set_msgcall()</a> method for more information).

The `DB_ENV->rep_stat_print()` method may not be called before the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called.

The `DB_ENV->rep_stat_print()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flags

The **flags** parameter must be set to 0 or by bitwise inclusively **OR**'ing together one or more of the following values:

- `DB_STAT_ALL`

  Display all available information.

- `DB_STAT_CLEAR`

  Reset statistics after displaying their values.

### Errors

The `DB_ENV->rep_stat_print()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called before <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
