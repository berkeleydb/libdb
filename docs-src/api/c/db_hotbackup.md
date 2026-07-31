---
title: "db_hotbackup"
api-name: "db_hotbackup"
source: docs/api_reference/C/db_hotbackup.html
---
## db_hotbackup

``` c
db_hotbackup [-cDEguVv] [-d data_dir ...] [-h home] 
        [-l log_dir] [-P password] -b backup_dir  
```

The <span class="command">**db_hotbackup**</span> utility creates "hot backup" or "hot failover" snapshots of Berkeley DB database environments. Hot backups can also be performed using the <a href="envbackup.md" class="xref" title="DB_ENV-&gt;backup()">DB_ENV-&gt;backup()</a> or <a href="envdbbackup.md" class="xref" title="DB_ENV-&gt;dbbackup()">DB_ENV-&gt;dbbackup()</a> methods.

The <span class="command">**db_hotbackup**</span> utility performs the following steps:

1.  Sets the <a href="envset_flags.md#set_flags_DB_HOTBACKUP_IN_PROGRESS" class="xref"><code class="literal">DB_HOTBACKUP_IN_PROGRESS</code></a> flag in the home database environment.

2.  If the **-c** option is specified, checkpoint the source home database environment, and remove any unnecessary log files.

3.  If the target directory for the backup does not exist, it is created with mode read-write-execute for the owner.

    If the target directory for the backup does exist and the **-u** option was specified, all log files in the target directory are removed; if the **-u** option was not specified, all files in the target directory are removed.

4.  If the **-u** option was not specified, copy application-specific files found in the database environment home directory, and any directories specified using the **-d** option, into the target directory for the backup.

5.  Copy all log files found in the directory specified by the **-l** option (or in the database environment home directory, if no **-l** option was specified), into the target directory for the backup.

6.  Perform catastrophic recovery in the target directory for the backup.

7.  Remove any unnecessary log files from the target directory for the backup.

8.  Reset the <a href="envset_flags.md#set_flags_DB_HOTBACKUP_IN_PROGRESS" class="xref"><code class="literal">DB_HOTBACKUP_IN_PROGRESS</code></a> flag in the environment.

The <span class="command">**db_hotbackup**</span> utility does not resolve pending transactions that are in the prepared state. Applications that use <a href="txnprepare.md" class="xref" title="DB_TXN-&gt;prepare()">DB_TXN-&gt;prepare()</a> must specify <a href="envopen.md#envopen_DB_RECOVER_FATAL" class="link">DB_RECOVER_FATAL</a> when opening the environment, and run <a href="txnrecover.md" class="xref" title="DB_ENV-&gt;txn_recover()">DB_ENV-&gt;txn_recover()</a> to resolve any pending transactions, when failing over to the backup.

The options are as follows:

- **-b**

  Specify the target directory for the backup.

- **-c**

  Before performing the backup, checkpoint the source database environment and remove any log files that are no longer required in that environment. **To avoid making catastrophic recovery impossible, log file removal must be integrated with log file archival.**

- **-D**

  Use the data and log directories listed in a <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> configuration file in the source directory. This option has four effects:

  - The specified data and log directories will be created relative to the target directory, with mode read-write-execute owner, if they do not already exist.

  - In step \#3 above, all files in any source data directories specified in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file will be copied to the target data directories.

  - In step \#4 above, log files will be copied from any log directory specified in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file, instead of from the default locations.

  - The <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> configuration file will be copied from the source directory to the target directory, and subsequently used for configuration if recovery is run in the target directory.

  Care should be taken with the **-D** option where data and log directories are named relative to the source directory but are not subdirectories (that is, the name includes the element "..") Specifically, the constructed target directory names must be meaningful and distinct from the source directory names, otherwise running recovery in the target directory might corrupt the source data files.

  **It is an error to use absolute pathnames for data or log directories in this mode, as the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> configuration file copied into the target directory would then point at the source directories and running recovery would corrupt the source data files.**

- **-d**

  Specify one or more directories that contain data files to be copied to the target directory.

  **As all database files are copied into a single target directory, files named the same, stored in different source directories, would overwrite each other when copied to the target directory.**

  Please note the database environment recovery log references database files as they are named by the application program. **If the application uses absolute or relative pathnames to name database files, (rather than filenames and the <a href="envset_data_dir.md" class="xref" title="DB_ENV-&gt;set_data_dir()">DB_ENV-&gt;set_data_dir()</a> method or the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> configuration file to specify filenames), running recovery in the target directory may not properly find the copies of the files or might even find the source files, potentially resulting in corruption.**

- **-F**

  Directly copy from the filesystem. This option can **CORRUPT** the backup if used while the environment is active and the operating system does not support atomic file system reads. This option is known to be safe only on UNIX systems, not Linux or Windows systems.

- **-g**

  Turn on debugging options. In particular this will leave the log files in the backup directory after running recovery.

- **-h**

  Specify the source directory for the backup. That is, the database environment home directory.

- **-l**

  Specify a source directory that contains log files; if none is specified, the database environment home directory will be searched for log files. If a relative path is specified, the path is evaluated relative to the home directory.

- **-P**

  Specify an environment password. Although Berkeley DB utilities overwrite password strings as soon as possible, be aware there may be a window of vulnerability on systems where unprivileged users can see command-line arguments or where utilities are not able to overwrite the memory containing the command-line arguments.

- **-u**

  Update a pre-existing hot backup snapshot by copying in new log files. If the **-u** option is specified, no databases will be copied into the target directory. If applications that update the environment are using the transactional bulk insert optimization, this option must be used with special care. For more information, see the section on Hot Backup in the <span class="emphasis">*Getting Started With Transaction Processing Guide*</span>.

- **-V**

  Write the library version number to the standard output, and exit.

- **-v**

  Run in verbose mode, listing operations as they are done.

The <span class="command">**db_hotbackup**</span> utility uses a Berkeley DB environment (as described for the **-h** option, the environment variable **DB_HOME**, or because the utility was run in a directory containing a Berkeley DB environment). In order to avoid environment corruption when using a Berkeley DB environment, <span class="command">**db_hotbackup**</span> should always be given the chance to detach from the environment and exit gracefully. To cause <span class="command">**db_hotbackup**</span> to release all environment resources and exit cleanly, send it an interrupt signal (SIGINT).

The <span class="command">**db_hotbackup**</span> utility exits 0 on success, and \>0 if an error occurs.

### Environment Variables

#### DB_HOME

If the **-h** option is not specified and the environment variable DB_HOME is set, it is used as the path of the database home, as described in the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method.
