---
title: "DB_ENV->lock_stat_print()"
api-name: "DB_ENV->lock_stat_print()"
source: docs/api_reference/C/lockstat_print.html
---
## DB_ENV-\>lock_stat_print()

``` c
#include <db.h>

int
DB_ENV->lock_stat_print(DB_ENV *env, u_int32_t flags);  
```

The `DB_ENV->lock_stat_print()` method displays the locking subsystem statistical information, as described for the `DB_ENV->lock_stat()` method. The information is printed to a specified output channel (see the <a href="envset_msgfile.md" class="xref" title="DB_ENV-&gt;set_msgfile()">DB_ENV-&gt;set_msgfile()</a> method for more information), or passed to an application callback function (see the <a href="envset_msgcall.md" class="xref" title="DB_ENV-&gt;set_msgcall()">DB_ENV-&gt;set_msgcall()</a> method for more information).

The `DB_ENV->lock_stat_print()` method may not be called before the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called.

The `DB_ENV->lock_stat_print()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flags

The **flags** parameter must be set to 0 or by bitwise inclusively **OR**'ing together one or more of the following values:

- `DB_STAT_ALL`

  Display all available information. For each object, the amount of data displayed is limited to 100 bytes, unless some other limit is set by calling <a href="envset_data_len.md" class="xref" title="DB_ENV-&gt;set_data_len()">DB_ENV-&gt;set_data_len()</a>, or by using the DB_CONFIG "set_data_len" parameter.

- `DB_STAT_CLEAR`

  Reset statistics after displaying their values.

- `DB_STAT_LOCK_CONF`

  Display the lock conflict matrix.

- `DB_STAT_LOCK_LOCKERS`

  Display the lockers within hash chains.

- `DB_STAT_LOCK_OBJECTS`

  Display the lock objects within hash chains. For each object, the amount of data displayed is limited to 100 bytes, unless some other limit is set by calling <a href="envset_data_len.md" class="xref" title="DB_ENV-&gt;set_data_len()">DB_ENV-&gt;set_data_len()</a>, or by using the DB_CONFIG "set_data_len" parameter.

- `DB_STAT_LOCK_PARAMS`

  Display the locking subsystem parameters.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="lock.md" class="link" title="Chapter 6.  The DB_LOCK Handle">DB_LOCK</a>

### See Also

<a href="lock.md#locklist" class="xref" title="Locking Subsystem and Related Methods">Locking Subsystem and Related Methods</a>
