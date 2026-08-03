---
title: "DB->upgrade()"
api-name: "DB->upgrade()"
source: docs/api_reference/C/dbupgrade.html
---
## DB-\>upgrade()

``` c
#include <db.h>

int
DB->upgrade(DB *db, const char *file, u_int32_t flags);  
```

The `DB->upgrade()` method upgrades all of the databases included in the file **file**, if necessary. If no upgrade is necessary, `DB->upgrade()` always returns success.

**Database upgrades are done in place and are destructive. For example, if pages need to be allocated and no disk space is available, the database may be left corrupted. Backups should be made before databases are upgraded. See <a href="../../guides/programmer_reference/am_upgrade.md" class="olink">Upgrading databases</a> for more information.**

Unlike all other database operations, `DB->upgrade()` may only be done on a system with the same byte-order as the database.

The `DB->upgrade()` method returns a non-zero error value on failure and 0 on success.

The `DB->upgrade()` method is the underlying method used by the <a href="db_upgrade.md" class="link" title="db_upgrade">db_upgrade</a> utility. See the <a href="db_upgrade.md" class="link" title="db_upgrade">db_upgrade</a> utility source code for an example of using `DB->upgrade()` in a IEEE/ANSI Std 1003.1 (POSIX) environment.

### Parameters

#### file

The **file** parameter is the physical file containing the databases to be upgraded.

#### flags

The **flags** parameter must be set to 0 or the following value:

- `DB_DUPSORT`

  **This flag is only meaningful when upgrading databases from releases before the Berkeley DB 3.1 release.**

  As part of the upgrade from the Berkeley DB 3.0 release to the 3.1 release, the on-disk format of duplicate data items changed. To correctly upgrade the format requires applications to specify whether duplicate data items in the database are sorted or not. Specifying the DB_DUPSORT flag informs `DB->upgrade()` that the duplicates are sorted; otherwise they are assumed to be unsorted. Incorrectly specifying the value of this flag may lead to database corruption.

  Further, because the `DB->upgrade()` method upgrades a physical file (including all the databases it contains), it is not possible to use `DB->upgrade()` to upgrade files in which some of the databases it includes have sorted duplicate data items, and some of the databases it includes have unsorted duplicate data items. If the file does not have more than a single database, if the databases do not support duplicate data items, or if all of the databases that support duplicate data items support the same style of duplicates (either sorted or unsorted), `DB->upgrade()` will work correctly as long as the DB_DUPSORT flag is correctly specified. Otherwise, the file cannot be upgraded using `DB->upgrade;()` it must be upgraded manually by dumping and reloading the databases.

### Environment Variables

If the database was opened within a database environment, the environment variable `DB_HOME` may be used as the path of the database environment home.

`DB->upgrade()` is affected by any database directory specified using the <a href="envset_data_dir.md" class="xref" title="DB_ENV-&gt;set_data_dir()">DB_ENV-&gt;set_data_dir()</a> method, or by setting the "set_data_dir" string in the environment's <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file.

### Errors

The `DB->upgrade()` method may fail and return one of the following non-zero errors:

#### DB_OLD_VERSION

The database cannot be upgraded by this version of the Berkeley DB software.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
