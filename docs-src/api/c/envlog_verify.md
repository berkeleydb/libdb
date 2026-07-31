---
title: "DB_ENV->log_verify()"
api-name: "DB_ENV->log_verify()"
source: docs/api_reference/C/envlog_verify.html
---
## DB_ENV-\>log_verify()

``` c
#include <db.h>
int
DB_ENV->log_verify(DB_ENV *dbenv, const DB_LOG_VERIFY_CONFIG *config); 
    
```

The `DB_ENV->log_verify()` method verifies the integrity of the log records of an environment and writes both error and normal messages to the error/message output facility of the database environment handle.

The `DB_ENV->log_verify()` method does not perform the locking function, even in Berkeley DB environments that are configured with a locking subsystem. Because this function does not access any database files, you can call it even when the environment has other threads of control attached and running.

The `DB_ENV->log_verify()` method is the underlying method used by the `DB_ENV-> db_log_verify` utility. See the `DB_ENV-> db_log_verify` utility source code for an example of using `DB_ENV->log_verify()` in a IEEE/ANSI Std 1003.1 (POSIX) environment.

The `DB_ENV->log_verify()` method returns DB_LOG_VERIFY_BAD when either log errors are detected or the internal data storage layer does not work. It returns EINVAL if you specify wrong configurations. Unless otherwise specified, the `DB_ENV->log_verify()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### config

The configuration parameter of type DB_LOG_VERIFY_CONFIG is for the verification of log files. A struct variable of this type must be memset to 0 before setting any configurations to it.

### DB_LOG_VERIFY_CONFIG members

``` c
struct __db_logvrfy_config {
int continue_after_fail, verbose;
u_int32_t cachesize;
const char *temp_envhome;
const char *dbfile, *dbname;
DB_LSN start_lsn, end_lsn;
time_t start_time, end_time;
};
```

#### continue_after_fail

The **continue_after_fail** parameter specifies whether or not continue the verification process when an error in the log is detected.

#### verbose

The **verbose** parameter specifies whether or not to display verbose output during the verification process.

#### cachesize

The **cachesize** parameter specifies the size of the cache of the temporary internal environment in bytes.

#### temp_envhome

The **temp_envhome** parameter is the home directory of the temporary database environment that is used internally during the verification. It can be NULL, meaning the environment and all databases are in-memory.

#### dbfile

The **dbfile** parameter specifies that for log records involving a database file, only those related to this database file are verified. Log records not involving database files are verified regardless of this parameter.

#### dbname

The **dbname** parameter specifies that for log records involving a database file, only those related to this database file are verified. Log records not involving database files are verified regardless of this parameter.

#### start_lsn and end_lsn

The **start_lsn** and **end_lsn** parameters specify the range of log records from the entire log set, that must be verified. Either of them can be \[0\]\[0\], to specify an open ended range. If both of them are \[0\]\[0\] (by default) the entire log is verified.

#### start_time and end_time

The **start_time** and **end_time** parameters specify range of log records from the entire log set that must be verified for a time range. Either of them can be 0, to specify an open ended range. If both of them are 0 (by default), the entire log is verified.

Note that the time range specified is not precise, because such a time range is converted to an lsn range based on the time points we know from transaction commits and checkpoints.

You can specify either an lsn range or a time range. You can neither specify both nor specify an lsn and a time as a range.

### Environment Variables

If the database is opened within a database environment, the environment variable `DB_HOME` can be used as the path of the database environment home.

### Errors

The `DB_ENV->log_verify()` method may fail and return one of the following non-zero errors:

EINVAL or DB_LOG_VERIFY_BAD.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
