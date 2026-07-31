---
title: "Chapter 5. Administrating Berkeley DB SQL Databases"
api-name: "Chapter 5. Administrating Berkeley DB SQL Databases"
source: docs/bdb-sql/admin.html
---
## Chapter 5. Administrating Berkeley DB SQL Databases

**Table of Contents**

<span class="sect1"> [Backing Up Berkeley DB SQL Databases](admin.md#backup) </span>

<span class="sect2"> [Backing Up Replicated Berkeley DB SQL Databases](admin.md#idp50739296) </span>

<span class="sect1"> [Syncing with Oracle Databases](sync.md) </span>

<span class="sect2"> [Syncing on Unix Platforms](sync.md#syncunix) </span>

<span class="sect2"> [Syncing on Windows Platforms](sync.md#syncwin) </span>

<span class="sect2"> [Syncing on Windows Mobile Platforms](sync.md#syncwinmobile) </span>

<span class="sect1"> [Data Migration](datamigration.md) </span>

<span class="sect2"> [Migration Using the Shells](datamigration.md#shellmigrate) </span>

This chapter provides administrative procedures that are unique to the Berkeley DB SQL interface.

## Backing Up Berkeley DB SQL Databases

<span class="sect2"> [Backing Up Replicated Berkeley DB SQL Databases](admin.md#idp50739296) </span>

You can use the standard SQLite `.dump` command to backup the data managed by the BDB SQL interface.

The BDB SQL interface supports the standard SQLite Online Backup API. However, there is a small difference between the two interfaces. In the BDB SQL interface, the value returned by the `sqlite3_backup_remaining` method and the number of pages passed to the `sqlite3_backup_step` method, are estimates of the number of pages to be copied and not exact values. To be certain that the backup process is complete, check if the `sqlite3_backup_step` method has returned `SQLITE_DONE`. To learn how to use SQLite Online Backup API, see the official <a href="http://www.sqlite.org/backup.html" class="ulink" target="_top">SQLite Documentation Page.</a>

If you are using replication, you will also need to copy the file that contains the replication pragma information in order to have a full backup. To do that copy the file named ` pragma` from the database journal directory.

### Backing Up Replicated Berkeley DB SQL Databases

When BDB SQL interface databases are replicated the process for backing up a regular database should be followed. The user must then copy some additional files for the backup to be complete.

The additional files can be found in the journal directory of the source database, and should be copied into the journal directory of the backup copy. The journal directory is automatically created when a Berkeley DB SQL interface database is created. The journal directory is created in the same directory as the database file, it has the name of the database file with a `-journal` appendix.

The files that need to be copied into the backup journal directory are:

- `__db.rep.egen`
- `__db.rep.gen`
- `__db.rep.init`
- `__db.rep.system`
