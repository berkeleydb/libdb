---
title: "DB->stat_print()"
api-name: "DB->stat_print()"
source: docs/api_reference/C/dbstat_print.html
---
## DB-\>stat_print()

``` c
#include <db.h>

int
DB->stat_print(DB *db, u_int32_t flags);  
```

The `DB->stat_print()` method displays the database statistical information, as described for the <a href="dbstat.md" class="xref" title="DB-&gt;stat()">DB-&gt;stat()</a> method. The information is printed to a specified output channel (see the <a href="envset_msgfile.md" class="xref" title="DB_ENV-&gt;set_msgfile()">DB_ENV-&gt;set_msgfile()</a> method for more information), or passed to an application callback function (see the <a href="envset_msgcall.md" class="xref" title="DB_ENV-&gt;set_msgcall()">DB_ENV-&gt;set_msgcall()</a> method for more information).

The `DB->stat_print()` method may not be called before the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called.

The `DB->stat_print()` method returns a non-zero error value on failure and 0 on success.

For Berkeley DB SQL table or index statistics, see <a href="dbsql.md#dbsql_command_feature" class="xref" title="Command Line Features Unique to dbsql">Command Line Features Unique to dbsql</a>.

### Parameters

#### flags

The **flags** parameter must be set to 0 or by bitwise inclusively **OR**'ing together one or more of the following values:

- `DB_FAST_STAT`

  Return only the values which do not require traversal of the database. Among other things, this flag makes it possible for applications to request key and record counts without incurring the performance penalty of traversing the entire database.

- `DB_STAT_ALL`

  Display all available information.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
