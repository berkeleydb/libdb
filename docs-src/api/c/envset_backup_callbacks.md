---
title: "DB_ENV->set_backup_callbacks()"
api-name: "DB_ENV->set_backup_callbacks()"
source: docs/api_reference/C/envset_backup_callbacks.html
---
## DB_ENV-\>set_backup_callbacks()

``` c
#include <db.h>
 
DB_ENV->set_backup_callbacks(DB_ENV, 
        int (*open_func)(DB_ENV *, const char *dbname, 
                         const char *target, void **handle),
        int (*write_func)(DB_ENV *, u_int32_t offset_gbytes, 
                          u_int32_t offset_bytes, u_int32_t size, 
                          u_int8_t *buf, void *handle),
        int (*close_func)(DB_ENV *, const char *dbname, void *handle));  
```

The `DB_ENV->set_backup_callbacks()` method configures three callback functions which can be used by the <a href="envbackup.md" class="xref" title="DB_ENV-&gt;backup()">DB_ENV-&gt;backup()</a> or <a href="envdbbackup.md" class="xref" title="DB_ENV-&gt;dbbackup()">DB_ENV-&gt;dbbackup()</a> methods to override their default behavior. If one callback is configured, then all three callbacks must be configured. These callbacks are required if the `target` parameter is set to `NULL` for the <a href="envbackup.md" class="xref" title="DB_ENV-&gt;backup()">DB_ENV-&gt;backup()</a> or <a href="envdbbackup.md" class="xref" title="DB_ENV-&gt;dbbackup()">DB_ENV-&gt;dbbackup()</a> methods.

The `DB_ENV->set_backup_callbacks()` method configures operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle, not all operations performed on the underlying database environment.

The `DB_ENV->set_backup_callbacks()` method may be called at any time during the life of the application.

The `DB_ENV->set_backup_callbacks()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### open_func

The **open_func** parameter is the function used when a target location is opened during a backup. This function should do whatever is necessary to prepare the backup destination for writing the data.

This function takes four parameters:

- `dbenv`

  The **dbenv** parameter is the enclosing database environment handle.

- `dbname`

  The **dbname** parameter is the name of the database being backed up.

- `target`

  The **target** parameter is the backup's directory destination.

- `handle`

  The **handle** parameter references the handle (usually a file handle) to which the backup will be written.

#### write_func

The **write_func** parameter is the function used to write data during a backup. The function takes six parameters:

- `dbenv`

  The **dbenv** parameter is the enclosing database environment handle.

- `offset_gbytes`

  The **offset_gbytes** parameter specifies the number of gigabytes into the output handle where the data can should be written. This value, plus the value specified on `offset_bytes`, indicates the offset within the output handle where the backup should begin.

- `offset_bytes`

  The **offset_bytes** parameter specifies the number of bytes into the output handle where the data can be located. This value, plus the value specified on `offset_gbytes`, indicates the offset within the output handle where the backup should begin.

- `size`

  The **size** parameter specifies the number of bytes to back up from the buffer.

- `buf`

  The **buf** parameter is the buffer which contains the data to be backed up.

- `handle`

  The **handle** parameter references the handle (usually a file handle) to which the backup will be written.

#### close_func

The **close_func** parameter is the function used when ending a backup and closing a backup target. The function takes three parameters:

- `dbenv`

  The **dbenv** parameter is the enclosing database environment handle.

- `dbname`

  The **dbname** parameter is the name of the database that has now been backed up.

- `handle`

  The **handle** parameter references the handle (usually a file handle) to which the backup was written, and which now must be closed or otherwise discarded.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>, <a href="envget_backup_callbacks.md" class="xref" title="DB_ENV-&gt;get_backup_callbacks()">DB_ENV-&gt;get_backup_callbacks()</a>, <a href="envbackup.md" class="xref" title="DB_ENV-&gt;backup()">DB_ENV-&gt;backup()</a>, and <a href="envdbbackup.md" class="xref" title="DB_ENV-&gt;dbbackup()">DB_ENV-&gt;dbbackup()</a>.
