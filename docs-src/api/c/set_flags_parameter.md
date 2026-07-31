---
title: "set_flags"
api-name: "set_flags"
source: docs/api_reference/C/set_flags_parameter.html
---
## set_flags

Configures a database environment.

The syntax of the entry in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `set_flags`, one or more whitespace characters, the method flag parameter as a string, optionally one or more whitespace characters, and the string `on` or `off`. If the optional string is omitted, the default is `on`; for example, `set_flags DB_TXN_NOSYNC` or `set_flags DB_TXN_NOSYNC on`. Because the DB_CONFIG file is read when the database environment is opened, it will silently overrule configuration done before that time.

The method flag parameters are as follows:

- DB_AUTO_COMMIT

  Enables/disables to automatically enclose those DB handle operations for which no explicit transaction handle was specified, and which modify databases in the database environment, within a transaction.

- DB_CDB_ALLDB

  Enables/disables Berkeley DB Concurrent Data Store applications to perform locking on an environment-wide basis rather than on a per-database basis.

- DB_DIRECT_DB

  Enables/disables turning off system buffering of Berkeley DB database files to avoid double caching.

- DB_DSYNC_DB

  Enables/disables configuring Berkeley DB to flush database writes to the backing disk before returning from the write system call, rather than flushing database writes explicitly in a separate system call, as necessary.

- DB_MULTIVERSION

  Enables/disables all databases in the environment from being opened as if DB_MULTIVERSION is passed to the DB-\>open method. This flag will be ignored for queue databases for which DB_MULTIVERSION is not supported.

- DB_NOMMAP

  Enables/disables Berkeley DB from copying read-only database files into the local cache instead of potentially mapping them into process memory.

- DB_REGION_INIT

  Enables/disables Berkeley DB to page-fault shared regions into memory when initially creating or joining a Berkeley DB environment. In addition, Berkeley DB will write the shared regions when creating an environment, forcing the underlying virtual memory and filesystems to instantiate both the necessary memory and the necessary disk space.

- DB_TIME_NOTGRANTED

  Enables/disables those database calls timing out based on lock or transaction timeout values to return DB_LOCK_NOTGRANTED instead of DB_LOCK_DEADLOCK. This allows applications to distinguish between operations which have deadlocked and operations which have exceeded their time limits.

- DB_TXN_NOSYNC

  Enables/disables Berkeley DB to not write or synchronously flush the log on transaction commit.

- DB_TXN_NOWAIT

  Enables/disables the operation to return DB_LOCK_DEADLOCK if a lock is unavailable for any Berkeley DB operation performed in the context of a transaction.

- DB_TXN_SNAPSHOT

  Enables/disables all transactions in the environment to be started as if DB_TXN_SNAPSHOT were passed to the DB_ENV-\>txn_begin method, and all non-transactional cursors to be opened as if DB_TXN_SNAPSHOT were passed to the DB-\>cursor method.

- DB_TXN_WRITE_NOSYNC

  Enables/disables Berkeley DB to write, but not synchronously flush, the log on transaction commit.

- DB_YIELDCPU

  Enables/disables Berkeley DB to yield the processor immediately after each page or mutex acquisition.

For more information, see <a href="envset_flags.md" class="xref" title="DB_ENV-&gt;set_flags()">DB_ENV-&gt;set_flags()</a>.
