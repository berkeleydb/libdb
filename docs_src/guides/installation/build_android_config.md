---
title: "Android Configuration Options"
api-name: "Android Configuration Options"
source: docs/installation/build_android_config.html
---
## Android Configuration Options

There are several configuration options you can specify in `LOCAL_CFLAGS` located in the `Android.mk` file.

- BDBSQL_CONVERT_SQLITE

  This option enables to convert SQLite database to BDB SQL database format. See <a href="build_android_intro.md#build_android_migrate" class="xref" title="Migrating from SQLite to Berkeley DB">Migrating from SQLite to Berkeley DB</a> for more information.

- BDBSQL_SHARE_PRIVATE

  This flag is enabled by default and keeps all the region files in memory instead of the disk. This flag also implements database-level locking.

- SQLITE_DEFAULT_CACHE_SIZE

  SQLite provides an in-memory cache which you size according to the maximum number of database pages that you want to hold in memory at any given time. See <a href="build_unix_sql.md#config_sql" class="xref" title="Changing Compile Options">Changing Compile Options</a>.

- SQLITE_DEFAULT_JOURNAL_SIZE_LIMIT

  For SQLite, this pragma identifies the maximum size that the journal file is allowed to be. Berkeley DB does not have a journal file, but it writes and uses log files. A new log file is created when the current log file has reached the defined maximum size. This flag defines this maximum size for a log file. Default value is 10 MB for Berkeley DB SQL interface.

Hard-coded numbers in the build can be adjusted using the following SQLite PRAGMA commands:

- PRAGMA cache_size
- PRAGMA journal_size_limit

You can configure most aspects of your Berkeley DB environment by using the <a href="../../api/c/configuration_reference.md" class="olink">DB_CONFIG</a> file.
