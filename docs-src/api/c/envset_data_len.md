---
title: "DB_ENV->set_data_len()"
api-name: "DB_ENV->set_data_len()"
source: docs/api_reference/C/envset_data_len.html
---
## DB_ENV-\>set_data_len()

``` c
#include <db.h>

int
DB_ENV->set_data_len(DB_ENV *dbenv, u_int32_t bytes);  
```

Limits the amount of data displayed when <a href="lockstat_print.md" class="xref" title="DB_ENV-&gt;lock_stat_print()">DB_ENV-&gt;lock_stat_print()</a> is called with the `DB_STAT_ALL` or `DB_STAT_LOCK_OBJECTS` flag.

This method is explicitly called in the <a href="db_printlog.md" class="xref" title="db_printlog">db_printlog</a> and <a href="db_dump.md" class="xref" title="db_dump">db_dump</a> utilities when using the **-D** command line option. When used in this manner it will set the maximum number of bytes to display for each key/data item. These utilities run in their own environment context.

If you want to call this method from the primary application and have it set the maximum number of bytes to display for each key/data item, then you must bring the db_dump/db_printlog code into the primary application and ensure that the same environment handle is used throughout.

This limit may also be configured using the environment's <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. In this case, the limit will equally affect your application code, as well as the command line utilities noted above without modification to their code. The syntax of the entry in that file is a single line with the string "set_data_len", one or more whitespace characters, and the limit in bytes that you want to set.

The `DB_ENV->set_data_len()` method configures operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle, not all operations performed on the underlying database environment.

The `DB_ENV->set_data_len()` method may be called at any time during the life of the application.

The `DB_ENV->set_data_len()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### bytes

The **bytes** parameter identifies the maximum number of bytes to display when dumping the database or printing the log. The value specified here must be greater than `0`.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
