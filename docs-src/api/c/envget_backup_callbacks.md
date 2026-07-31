---
title: "DB_ENV->get_backup_callbacks()"
api-name: "DB_ENV->get_backup_callbacks()"
source: docs/api_reference/C/envget_backup_callbacks.html
---
## DB_ENV-\>get_backup_callbacks()

``` c
#include <db.h>
 
DB_ENV->get_backup_callbacks(DB_ENV, 
        int (**open_func)(DB_ENV *, const char *dbname, 
                          const char *target, void **handle),
        int (**write_func)(DB_ENV *, u_int32_t offset_gbytes, 
                           u_int32_t offset_bytes, u_int32_t size, 
                           u_int8_t *buf, void *handle),
        int (**close_func)(DB_ENV *, const char *dbname, void *handle));  
```

The `DB_ENV->get_backup_callbacks()` method retrieves the three callback functions which can be used by the <a href="envbackup.md" class="xref" title="DB_ENV-&gt;backup()">DB_ENV-&gt;backup()</a> or <a href="envdbbackup.md" class="xref" title="DB_ENV-&gt;dbbackup()">DB_ENV-&gt;dbbackup()</a> methods to override their default behavior. These callbacks are configured using the <a href="envset_backup_callbacks.md" class="xref" title="DB_ENV-&gt;set_backup_callbacks()">DB_ENV-&gt;set_backup_callbacks()</a> method.

The `DB_ENV->get_backup_callbacks()` method may be called at any time during the life of the application.

The `DB_ENV->get_backup_callbacks()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### open_func

The **open_func** parameter is the function used when a target location is opened during a backup.

#### write_func

The **close_func** parameter is the function used to write data during a backup.

#### close_func

The **close_func** parameter is the function used when ending a backup and closing a backup target.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>, <a href="envset_backup_callbacks.md" class="xref" title="DB_ENV-&gt;set_backup_callbacks()">DB_ENV-&gt;set_backup_callbacks()</a>, <a href="envbackup.md" class="xref" title="DB_ENV-&gt;backup()">DB_ENV-&gt;backup()</a>, and <a href="envdbbackup.md" class="xref" title="DB_ENV-&gt;dbbackup()">DB_ENV-&gt;dbbackup()</a>.
