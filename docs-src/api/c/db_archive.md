---
title: "db_archive"
api-name: "db_archive"
source: docs/api_reference/C/db_archive.html
---
## db_archive

``` c
db_archive [-adlsVv] [-h home] [-P password]  
```

The <span class="command">**db_archive**</span> utility writes the pathnames of log files that are no longer in use (for example, no longer involved in active transactions), to the standard output, one pathname per line. These log files should be written to backup media to provide for recovery in the case of catastrophic failure (which also requires a snapshot of the database files), but they may then be deleted from the system to reclaim disk space.

### Note

If the application(s) that use the environment make use of any of the following methods:

|  |
|----|
| <a href="envadd_data_dir.md" class="xref" title="DB_ENV-&gt;add_data_dir()">DB_ENV-&gt;add_data_dir()</a> |
| <a href="envset_data_dir.md" class="xref" title="DB_ENV-&gt;set_data_dir()">DB_ENV-&gt;set_data_dir()</a> |
| <a href="envset_lg_dir.md" class="xref" title="DB_ENV-&gt;set_lg_dir()">DB_ENV-&gt;set_lg_dir()</a> |

then in order for this utility to run correctly, you need a <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file which sets the proper paths using the <a href="add_data_dir_parameter.md" class="xref" title="add_data_dir">add_data_dir</a>, or <a href="set_lg_dir_parameter.md" class="xref" title="set_lg_dir">set_lg_dir</a> configuration parameters.

The options are as follows:

- **-a**

  Write all pathnames as absolute pathnames, instead of relative to the database home directory.

- **-d**

  Remove log files that are no longer needed; no filenames are written. This automatic log file removal is likely to make catastrophic recovery impossible.

- **-h**

  Specify a home directory for the database environment; by default, the current working directory is used.

- **-l**

  Write out the pathnames of all the database log files, whether or not they are involved in active transactions.

- **-P**

  Specify an environment password. Although Berkeley DB utilities overwrite password strings as soon as possible, be aware there may be a window of vulnerability on systems where unprivileged users can see command-line arguments or where utilities are not able to overwrite the memory containing the command-line arguments.

- **-s**

  Write the pathnames of all the database files that need to be archived in order to recover the database from catastrophic failure. If any of the database files have not been accessed during the lifetime of the current log files, <span class="command">**db_archive**</span> will not include them in this output.

  It is possible that some of the files to which the log refers have since been deleted from the system. In this case, <span class="command">**db_archive**</span> will ignore them. When <a href="db_recover.md" class="xref" title="db_recover">db_recover</a> is run, any files to which the log refers that are not present during recovery are assumed to have been deleted and will not be recovered.

- **-V**

  Write the library version number to the standard output, and exit.

- **-v**

  Run in verbose mode.

Log cursor handles (returned by the <a href="logcursor.md" class="xref" title="DB_ENV-&gt;log_cursor()">DB_ENV-&gt;log_cursor()</a> method) may have open file descriptors for log files in the database environment. Also, the Berkeley DB interfaces to the database environment logging subsystem (for example, <a href="logput.md" class="xref" title="DB_ENV-&gt;log_put()">DB_ENV-&gt;log_put()</a> and <a href="txnabort.md" class="xref" title="DB_TXN-&gt;abort()">DB_TXN-&gt;abort()</a> may allocate log cursors and have open file descriptors for log files as well. On operating systems where filesystem related system calls (for example, rename and unlink on Windows/NT) can fail if a process has an open file descriptor for the affected file, attempting to move or remove the log files listed by <span class="command">**db_archive**</span> may fail. All Berkeley DB internal use of log cursors operates on active log files only and furthermore, is short-lived in nature. So, an application seeing such a failure should be restructured to close any open log cursors it may have, and otherwise to retry the operation until it succeeds. (Although the latter is not likely to be necessary; it is hard to imagine a reason to move or rename a log file in which transactions are being logged or aborted.)

The <span class="command">**db_archive**</span> utility uses a Berkeley DB environment (as described for the **-h** option, the environment variable **DB_HOME**, or because the utility was run in a directory containing a Berkeley DB environment). In order to avoid environment corruption when using a Berkeley DB environment, <span class="command">**db_archive**</span> should always be given the chance to detach from the environment and exit gracefully. To cause <span class="command">**db_archive**</span> to release all environment resources and exit cleanly, send it an interrupt signal (SIGINT).

The <a href="logarchive.md" class="xref" title="DB_ENV-&gt;log_archive()">DB_ENV-&gt;log_archive()</a> method is the underlying method used by the <span class="command">**db_archive**</span> utility. See the <span class="command">**db_archive**</span> utility source code for an example of using `DB_ENV->log_archive()` in an IEEE/ANSI Std 1003.1 (POSIX) environment.

The <span class="command">**db_archive**</span> utility exits 0 on success, and \>0 if an error occurs.

### Environment Variables

#### DB_HOME

If the **-h** option is not specified and the environment variable DB_HOME is set, it is used as the path of the database home, as described in the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method.
