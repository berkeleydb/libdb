---
title: "DB_ENV->get_backup_config()"
api-name: "DB_ENV->get_backup_config()"
source: docs/api_reference/C/envget_backup_config.html
---
## DB_ENV-\>get_backup_config()

``` c
#include <db.h>

DB_ENV->get_backup_config(DB_ENV, db_backup_config_t option, 
                          u_int32_t *valuep);  
```

The `DB_ENV->get_backup_config()` method retrieves the value set for hot backup tuning parameters. See the <a href="envbackup.md" class="xref" title="DB_ENV-&gt;backup()">DB_ENV-&gt;backup()</a> and <a href="envdbbackup.md" class="xref" title="DB_ENV-&gt;dbbackup()">DB_ENV-&gt;dbbackup()</a> methods for a description of the hot backup APIs. These tuning parameters can be set using the <a href="envset_backup_config.md" class="xref" title="DB_ENV-&gt;set_backup_config()">DB_ENV-&gt;set_backup_config()</a> method.

The `DB_ENV->get_backup_config()` method may be called at any time during the life of the application.

The `DB_ENV->get_backup_config()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### option

The **option** parameter identifies the backup parameter to be retrieved. It must be one of the following:

- `DB_BACKUP_WRITE_DIRECT`

  Turning this on causes direct I/O to be used when writing pages to the disk.

- `DB_BACKUP_READ_COUNT`

  Configures the number of pages to read before pausing.

- `DB_BACKUP_READ_SLEEP`

  Configures the number of microseconds to sleep between batches of reads.

- `DB_BACKUP_SIZE`

  Configures the size of the buffer, in megabytes, to read from the database.

#### valuep

The **valuep** parameter references memory into which is copied the current value of the backup tuning parameter identified by the **option** parameter.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>,

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>, <a href="envset_backup_config.md" class="xref" title="DB_ENV-&gt;set_backup_config()">DB_ENV-&gt;set_backup_config()</a>, <a href="envbackup.md" class="xref" title="DB_ENV-&gt;backup()">DB_ENV-&gt;backup()</a>, <a href="envdbbackup.md" class="xref" title="DB_ENV-&gt;dbbackup()">DB_ENV-&gt;dbbackup()</a>
