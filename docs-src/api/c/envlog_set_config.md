---
title: "DB_ENV->log_set_config()"
api-name: "DB_ENV->log_set_config()"
source: docs/api_reference/C/envlog_set_config.html
---
## DB_ENV-\>log_set_config()

``` c
#include <db.h>

int
DB_ENV->log_set_config(DB_ENV *dbenv, u_int32_t flags, int onoff);  
```

The `DB_ENV->log_set_config()` method configures the Berkeley DB logging subsystem.

The `DB_ENV->log_set_config()` method configures a database environment, not only operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle.

The `DB_ENV->log_set_config()` method may be called at any time during the life of the application.

The `DB_ENV->log_set_config()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flags

The **flags** parameter must be set by bitwise inclusively **OR**'ing together one or more of the following values:

- `DB_LOG_DIRECT`

  Turn off system buffering of Berkeley DB log files to avoid double caching.

  Calling `DB_ENV->log_set_config()` with the DB_LOG_DIRECT flag only affects the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle (and any other Berkeley DB handles opened within the scope of that handle). For consistent behavior across the environment, all <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handles opened in the environment must either set the DB_LOG_DIRECT flag or the flag should be specified in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> configuration file.

  The `DB_LOG_DIRECT` flag may be used to configure Berkeley DB at any time during the life of the application.

- `DB_LOG_DSYNC`

  Configure Berkeley DB to flush log writes to the backing disk before returning from the write system call, rather than flushing log writes explicitly in a separate system call, as necessary. This is only available on some systems (for example, systems supporting the IEEE/ANSI Std 1003.1 (POSIX) standard O_DSYNC flag, or systems supporting the Windows FILE_FLAG_WRITE_THROUGH flag). This flag may result in inaccurate file modification times and other file-level information for Berkeley DB log files. This flag may offer a performance increase on some systems and a performance decrease on others.

  Calling `DB_ENV->log_set_config()` with the DB_LOG_DSYNC flag only affects the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle (and any other Berkeley DB handles opened within the scope of that handle). For consistent behavior across the environment, all <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handles opened in the environment must either set the DB_LOG_DSYNC flag or the flag should be specified in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> configuration file.

  The `DB_LOG_DSYNC` flag may be used to configure Berkeley DB at any time during the life of the application.

- `DB_LOG_AUTO_REMOVE`

  If set, Berkeley DB will automatically remove log files that are no longer needed.

  Automatic log file removal is likely to make catastrophic recovery impossible.

  Replication Manager applications operate in a group-aware manner for log file removal, and automatic log file removal simplifies the application.

  Replication Base API applications will rarely want to configure automatic log file removal as it increases the likelihood a master will be unable to satisfy a client's request for a recent log record.

  Calling `DB_ENV->log_set_config()` with the `DB_LOG_AUTO_REMOVE` flag affects the database environment, including all threads of control accessing the database environment.

  The `DB_LOG_AUTO_REMOVE` flag may be used to configure Berkeley DB at any time during the life of the application.

- `DB_LOG_IN_MEMORY`

  If set, maintain transaction logs in memory rather than on disk. This means that transactions exhibit the ACI (atomicity, consistency, and isolation) properties, but not D (durability); that is, database integrity will be maintained, but if the application or system fails, integrity will not persist. All database files must be verified and/or restored from a replication group master or archival backup after application or system failure.

  When in-memory logs are configured and no more log buffer space is available, Berkeley DB methods may return an additional error value, `DB_LOG_BUFFER_FULL`. When choosing log buffer and file sizes for in-memory logs, applications should ensure the in-memory log buffer size is large enough that no transaction will ever span the entire buffer, and avoid a state where the in-memory buffer is full and no space can be freed because a transaction that started in the first log "file" is still active.

  Calling `DB_ENV->log_set_config()` with the `DB_LOG_IN_MEMORY` flag affects the database environment, including all threads of control accessing the database environment.

  The `DB_LOG_IN_MEMORY` flag may be used to configure Berkeley DB only before the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called.

- `DB_LOG_ZERO`

  If set, zero all pages of a log file when that log file is created. This has shown to provide greater transaction throughput in some environments. The log file will be zeroed by the thread which needs to re-create the new log file. Other threads may not write to the log file while this is happening.

  Calling `DB_ENV->log_set_config()` with the `DB_LOG_ZERO` flag affects only the current environment handle.

  The `DB_LOG_ZERO` flag may be used to configure Berkeley DB at any time.

#### onoff

If the **onoff** parameter is zero, the specified flags are cleared; otherwise they are set.

### Errors

The `DB_ENV->log_set_config()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="lsn.md#loglist" class="xref" title="Logging Subsystem and Related Methods">Logging Subsystem and Related Methods</a>
