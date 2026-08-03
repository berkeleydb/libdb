---
title: "db_recover"
api-name: "db_recover"
source: docs/api_reference/C/db_recover.html
---
## db_recover

``` c
db_recover [-cefVv] [-h home] [-P password] [-t [[CC]YY]MMDDhhmm[.SS]]]  
```

The <span class="command">**db_recover**</span> utility must be run after an unexpected application, Berkeley DB, or system failure to restore the database to a consistent state. All committed transactions are guaranteed to appear after <span class="command">**db_recover**</span> has run, and all uncommitted transactions will be completely undone.

Note that this utility performs the same action as if the environment is opened with the <a href="envopen.md#envopen_DB_RECOVER" class="link">DB_RECOVER</a> flag. If `DB_RECOVER` is specified on environment open, then use of this utility is not necessary.

### Note

If the application(s) that use the environment make use of any of the following methods:

|  |
|----|
| <a href="envadd_data_dir.md" class="xref" title="DB_ENV-&gt;add_data_dir()">DB_ENV-&gt;add_data_dir()</a> |
| <a href="envset_data_dir.md" class="xref" title="DB_ENV-&gt;set_data_dir()">DB_ENV-&gt;set_data_dir()</a> |
| <a href="envset_lg_dir.md" class="xref" title="DB_ENV-&gt;set_lg_dir()">DB_ENV-&gt;set_lg_dir()</a> |

then in order for this utility to run correctly, you need a <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file which sets the proper paths using the <a href="add_data_dir_parameter.md" class="xref" title="add_data_dir">add_data_dir</a>, or <a href="set_lg_dir_parameter.md" class="xref" title="set_lg_dir">set_lg_dir</a> configuration parameters.

The options are as follows:

- **-c**

  Perform catastrophic recovery instead of normal recovery.

- **-e**

  Retain the environment after running recovery. This option will rarely be used unless a <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is present in the home directory. If a <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is not present, then the regions will be created with default parameter values.

- **-f**

  Display a message on the standard output showing the percent of recovery completed.

- **-h**

  Specify a home directory for the database environment; by default, the current working directory is used.

- **-P**

  Specify an environment password. Although Berkeley DB utilities overwrite password strings as soon as possible, be aware there may be a window of vulnerability on systems where unprivileged users can see command-line arguments or where utilities are not able to overwrite the memory containing the command-line arguments.

- **-t**

  Recover to the time specified rather than to the most current possible date. The timestamp argument should be in the form `[[CC]YY]MMDDhhmm[.SS]` where each pair of letters represents the following:

  - **CC**

    The first two digits of the year (the century).

  - **YY**

    The second two digits of the year. If "YY" is specified, but "CC" is not, a value for "YY" between 69 and 99 results in a "CC" value of 19. Otherwise, a "CC" value of 20 is used.

  - **MM**

    The month of the year, from 1 to 12.

  - **DD**

    The day of the month, from 1 to 31.

  - **hh**

    The hour of the day, from 0 to 23.

  - **mm**

    The minute of the hour, from 0 to 59.

  - **SS**

    The second of the minute, from 0 to 61.

  If the "CC" and "YY" letter pairs are not specified, the values default to the current year. If the "SS" letter pair is not specified, the value defaults to 0.

- **-V**

  Write the library version number to the standard output, and exit.

- **-v**

  Run in verbose mode.

In the case of catastrophic recovery, an archival copy — or <span class="emphasis">*snapshot*</span> — of all database files must be restored along with all of the log files written since the database file snapshot was made. (If disk space is a problem, log files may be referenced by symbolic links). For further information on creating a database snapshot, see <a href="../../guides/programmer_reference/transapp_archival.md" class="olink">Archival Procedures</a>. For further information on performing recovery, see <a href="../../guides/programmer_reference/transapp_recovery.md" class="olink">Recovery Procedures</a>.

If the failure was not catastrophic, the files present on the system at the time of failure are sufficient to perform recovery.

If log files are missing, <span class="command">**db_recover**</span> will identify the missing log file(s) and fail, in which case the missing log files need to be restored and recovery performed again.

The <span class="command">**db_recover**</span> utility uses a Berkeley DB environment (as described for the **-h** option, the environment variable **DB_HOME**, or because the utility was run in a directory containing a Berkeley DB environment). In order to avoid environment corruption when using a Berkeley DB environment, <span class="command">**db_recover**</span> should always be given the chance to detach from the environment and exit gracefully. To cause <span class="command">**db_recover**</span> to release all environment resources and exit cleanly, send it an interrupt signal (SIGINT).

The <span class="command">**db_recover**</span> utility exits 0 on success, and \>0 if an error occurs.

### Environment Variables

#### DB_HOME

If the **-h** option is not specified and the environment variable DB_HOME is set, it is used as the path of the database home, as described in the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method.
