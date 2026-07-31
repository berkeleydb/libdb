---
title: "db_log_verify"
api-name: "db_log_verify"
source: docs/api_reference/C/db_log_verify.html
---
## db_log_verify

``` c
 
    
    db_log_verify [-cNvV] [-h home to verify] [-H temporary home] 
    [-P password] [-C cache size]
    [-b start lsn] [-e end lsn] [-s start time] [-z end time]
    [-d database file name] [-D database name]  
    
```

The <span class="command">**db_log_verify**</span> utility verifies the log files of a specific database environment. This utility verifies a specific range of log records, or changed log records of a specific database.

### Note

If the application(s) that use the environment make use of the <a href="envset_lg_dir.md" class="xref" title="DB_ENV-&gt;set_lg_dir()">DB_ENV-&gt;set_lg_dir()</a> method, then in order for this utility to run correctly, you need a <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file which sets the proper paths using the <a href="set_lg_dir_parameter.md" class="xref" title="set_lg_dir">set_lg_dir</a> configuration parameter.

The options are as follows:

- **-C**

  Specify the cache size (in megabytes) of the temporary database environment internally used during the log verification.

- **-b**

  Specify the starting log record (by lsn) to verify.

- **-c**

  Specify whether to continue the verification after an error is detected. If not specified, the verification stops when the first error is detected.

- **-D**

  Specify a database name. Only log records related to this database are verified.

- **-d**

  Specify a database file name. Only log records related this database file are verified.

- **-e**

  Specify the ending log record by lsn.

- **-h**

  Specify a home directory of the database environment whose log is to be verified.

- **-H**

  Specify a home directory for this utility to create a temporarily database environment to store runtime data during the verification.

  It is an error to specify the same directory as the -h option. If this directory is not specified, all temporary databases created during the verification will be in-memory, which is not a problem if the log files to verify are not huge.

- **-N**

  Do not acquire shared region mutexes while running. Other problems, such as potentially fatal errors in Berkeley DB, are ignored as well. This option is intended only for debugging errors, and should not be used under any other circumstances.

- **-P**

  Specify an environment password. Although Berkeley DB utilities overwrite password strings as soon as possible, there may be a window of vulnerability on systems where unprivileged users can see command-line arguments or where utilities are not able to overwrite the memory containing the command-line arguments.

- **-s**

  Specify the starting log record by time. The time range specified is not precise because the lsn of the most recent time point is used as the starting lsn.

- **-V**

  Write the library version number to the standard output and exit.

- **-v**

  Enable verbose mode to display verbose output during the verification process.

- **-z**

  Specify the ending log record by time. The time range specified is not precise because the lsn of the most recent time point is used as the ending lsn.

To specify a range of log records, you must provide either an lsn range or a time range. You can neither specify both nor specify an lsn and a time as a range.

If the log footprint is over several megabytes, specify a home directory and a big cache size for log verification internal use. Else, the process' private memory may be exhausted before the verification completes.

The <span class="command">**db_log_verify**</span> utility does not perform the locking function, even in Berkeley DB environments that are configured with a locking subsystem. All errors are written to stderr, and all normal and verbose messages are written to stdout.

The <span class="command">**db_log_verify**</span> utility can be used with a Berkeley DB environment (as described for the **-h** option, the environment variable **DB_HOME**). To avoid environment corruption when using a Berkeley DB environment, <span class="command">**db_log_verify**</span> must be given the chance to detach from the environment and exit gracefully. For the <span class="command">**db_log_verify**</span> utility to release all environment resources and exit, send an interrupt signal (SIGINT) to it.

The <span class="command">**db_log_verify**</span> utility returns a non-zero error value on failure and 0 on success.

### Environment Variables

#### DB_HOME

If the **-h** option is not specified and the environment variable DB_HOME is set, it is used as the path of the database home, as described in the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method.
