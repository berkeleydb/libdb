---
title: "DB_ENV->set_lg_max()"
api-name: "DB_ENV->set_lg_max()"
source: docs/api_reference/C/envset_lg_max.html
---
## DB_ENV-\>set_lg_max()

``` c
#include <db.h>

int
DB_ENV->set_lg_max(DB_ENV *dbenv, u_int32_t lg_max);  
```

Sets the maximum size of a single file in the log, in bytes. Because <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a> file offsets are unsigned four-byte values, the set value may not be larger than the maximum unsigned four-byte value.

When the logging subsystem is configured for on-disk logging, the default size of a log file is 10MB.

When the logging subsystem is configured for in-memory logging, the default size of a log file is 256KB. In addition, the configured log buffer size must be larger than the log file size. (The logging subsystem divides memory configured for in-memory log records into "files", as database environments configured for in-memory log records may exchange log records with other members of a replication group, and those members may be configured to store log records on-disk.) When choosing log buffer and file sizes for in-memory logs, applications should ensure the in-memory log buffer size is large enough that no transaction will ever span the entire buffer, and avoid a state where the in-memory buffer is full and no space can be freed because a transaction that started in the first log "file" is still active.

See <a href="../../programmer_reference/log_limits.html" class="olink">Log File Limits</a> for more information.

The database environment's log file size may also be configured using the environment's <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "set_lg_max", one or more whitespace characters, and the size in bytes. Because the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

The `DB_ENV->set_lg_max()` method configures a database environment, not only operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle.

The `DB_ENV->set_lg_max()` method may be called at any time during the life of the application.

If no size is specified by the application, the size last specified for the database region will be used, or if no database region previously existed, the default will be used.

The `DB_ENV->set_lg_max()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### lg_max

The **lg_max** parameter is the size of a single log file, in bytes.

### Errors

The `DB_ENV->set_lg_max()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the size of the log file is less than four times the size of the in-memory log buffer; the specified log file size was too large; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="logc.md" class="link" title="The DB_LOGC Handle">DB_LOGC</a>, <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a>

### See Also

<a href="lsn.md#loglist" class="xref" title="Logging Subsystem and Related Methods">Logging Subsystem and Related Methods</a>
