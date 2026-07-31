---
title: "DB_ENV->backup()"
api-name: "DB_ENV->backup()"
source: docs/api_reference/C/envbackup.html
---
## DB_ENV-\>backup()

``` c
#include <db.h>

int
DB_ENV->backup(DB_ENV *dbenv, const char *target, u_int32_t flags); 
```

The `DB_ENV->backup()` method performs a hot backup of the open environment. All files used by the environment are backed up, so long as the normal rules for file placement are followed. For information on how files are normally placed relative to the environment directory, see <a href="../../guides/programmer_reference/env_naming.md" class="olink">Berkeley DB File Naming</a> in the *Berkeley DB Programmer's Reference Guide*.

By default, data directories and the log directory specified relative to the home directory will be recreated relative to the target directory. If absolute path names are used, then specify `DB_BACKUP_SINGLE_DIR` to the `flags` parameter.

This method provides the same functionality as the <a href="db_hotbackup.md" class="xref" title="db_hotbackup">db_hotbackup</a> utility. However, this method does not perform the housekeeping actions performed by the `db_hotbackup` utility. In particular, you may want to run a checkpoint before calling this method. To run a checkpoint, use the <a href="txncheckpoint.md" class="xref" title="DB_ENV-&gt;txn_checkpoint()">DB_ENV-&gt;txn_checkpoint()</a> method. For more information on checkpoints, see <a href="../../guides/programmer_reference/transapp_checkpoint.md" class="olink">Checkpoints</a> in the *Berkeley DB Programmer's Reference Guide*.

To back up a single database file contained within the environment, use the <a href="envdbbackup.md" class="xref" title="DB_ENV-&gt;dbbackup()">DB_ENV-&gt;dbbackup()</a> method.

This method's default behavior can be changed by setting backup callbacks. See <a href="envset_backup_callbacks.md" class="xref" title="DB_ENV-&gt;set_backup_callbacks()">DB_ENV-&gt;set_backup_callbacks()</a> for more information. Additional tuning parameters can also be set using the <a href="envset_backup_config.md" class="xref" title="DB_ENV-&gt;set_backup_config()">DB_ENV-&gt;set_backup_config()</a> method.

The `DB_ENV->backup()` method may only be called after the environment handle has been opened.

The `DB_ENV->backup()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### target

Identifies the directory in which the back up will be placed. Any subdirectories required to contain the backup must be placed relative to this directory. Note that if the backup callbacks are set, then the value specified to this parameter is passed on to the `open_func()` callback. If this parameter is NULL, then the target must be specified to the `open_func()` callback.

This directory, and any required subdirectories, will be created for you if you specify the `DB_CREATE` flag on the call to this method. Otherwise, if the target does not exist, this method exits with an `ENOENT` error return.

#### flags

The **flags** parameter must be set to 0 or by bitwise inclusively **OR**'ing together one or more of the values:

- `DB_BACKUP_CLEAN`

  Before performing the backup, first remove all files from the target backup directory tree.

- `DB_BACKUP_FILES`

  Back up all ordinary files that might exist in the environment, and the environment's subdirectories.

- `DB_BACKUP_NO_LOGS`

  Back up only the `*.db` files. Do not backup the log files.

- `DB_BACKUP_SINGLE_DIR`

  Regardless of the directory structure used by the source environment, place all back up files in the single directory identified by the `target` parameter. Use this option if absolute path names to your environment directory and the files within that directory are required by your application.

- `DB_BACKUP_UPDATE`

  Perform an incremental back up, instead of a full back up. When this option is specified, only log files are copied to the target directory.

- `DB_CREATE`

  If the target directory does not exist, create it and any required subdirectories.

- `DB_EXCL`

  Return an `EEXIST` error if a target backup file already exists.

- `DB_VERB_BACKUP`

  Run in verbose mode, listing operations as they are completed.

### Errors

The `DB_ENV->backup()` method may fail and return one of the following non-zero errors:

#### EEXIST

`DB_EXCL` was specified for the `flags` parameter, and an existing target file was discovered when attempting to back up a source file.

#### ENOENT

The target directory does not exist and `DB_CREATE` was not specified for the `flags` parameter.

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
