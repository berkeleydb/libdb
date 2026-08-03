---
title: "DB_ENV->log_get_config()"
api-name: "DB_ENV->log_get_config()"
source: docs/api_reference/C/envlog_get_config.html
---
## DB_ENV-\>log_get_config()

``` c
#include <db.h>

int
DB_ENV->log_get_config(DB_ENV *dbenv, u_int32_t which, int *onoffp);  
```

The `DB_ENV->log_get_config()` method returns whether the specified **which** parameter is currently set or not. You can manage this value using the <a href="envlog_set_config.md" class="xref" title="DB_ENV-&gt;log_set_config()">DB_ENV-&gt;log_set_config()</a> method.

The `DB_ENV->log_get_config()` method may be called at any time during the life of the application.

The `DB_ENV->log_get_config()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### which

The **which** parameter is the message value for which configuration is being checked. Must be set to one of the following values:

- `DB_LOG_DIRECT`

  System buffering is turned off for Berkeley DB log files to avoid double caching.

- `DB_LOG_DSYNC`

  Berkeley DB is configured to flush log writes to the backing disk before returning from the write system call, rather than flushing log writes explicitly in a separate system call, as necessary.

- `DB_LOG_AUTO_REMOVE`

  Berkeley DB automatically removes log files that are no longer needed.

- `DB_LOG_IN_MEMORY`

  Transaction logs are maintained in memory rather than on disk. This means that transactions exhibit the ACI (atomicity, consistency, and isolation) properties, but not D (durability).

- `DB_LOG_ZERO`

  All pages of a log file are zeroed when that log file is created.

#### onoffp

The **onoffp** parameter references memory into which the configuration of the specified **which** parameter is copied.

If the returned **onoff** value is zero, the parameter is off; otherwise, on.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="lsn.md#loglist" class="xref" title="Logging Subsystem and Related Methods">Logging Subsystem and Related Methods</a>, <a href="envlog_set_config.md" class="xref" title="DB_ENV-&gt;log_set_config()">DB_ENV-&gt;log_set_config()</a>
