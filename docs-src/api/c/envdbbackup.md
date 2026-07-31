---
title: "DB_ENV->dbbackup()"
api-name: "DB_ENV->dbbackup()"
source: docs/api_reference/C/envdbbackup.html
---
## DB_ENV-\>dbbackup()

``` c
#include <db.h>

int
DB_ENV->dbbackup(DB_ENV *dbenv, const char *dbfile, const char *target, 
                 u_int32_t flags); 
```

The `DB_ENV->dbbackup()` method performs a hot backup of a single database file contained within the environment.

To back up an entire environment, use the <a href="envbackup.md" class="xref" title="DB_ENV-&gt;backup()">DB_ENV-&gt;backup()</a> method.

This method's default behavior can be changed by setting backup callbacks. See <a href="envset_backup_callbacks.md" class="xref" title="DB_ENV-&gt;set_backup_callbacks()">DB_ENV-&gt;set_backup_callbacks()</a> for more information. Additional tuning parameters can also be set using the <a href="envset_backup_config.md" class="xref" title="DB_ENV-&gt;set_backup_config()">DB_ENV-&gt;set_backup_config()</a> method.

The `DB_ENV->dbbackup()` method may only be called after the environment handle has been opened.

The `DB_ENV->dbbackup()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### dbfile

Identifies the database file that you want to back up.

#### target

Identifies the directory in which the back up will be placed. This target must exist; otherwise this method exits with an `ENOENT` error return.

Note that if the backup callbacks are set, then the value specified to this parameter is passed on to the `open_func()` callback. If this parameter is NULL, then the target must be specified directly to the `open_func()` callback.

#### flags

The **flags** parameter must be set to 0 or the following value:

- `DB_EXCL`

  Return an `EEXIST` error if a target backup file already exists.

### Errors

The `DB_ENV->dbbackup()` method may fail and return one of the following non-zero errors:

#### EEXIST

`DB_EXCL` was specified for the `flags` parameter, and an existing target file was discovered when attempting to back up a source file.

#### ENOENT

The target directory does not exist.

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
