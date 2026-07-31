---
title: "db_checkpoint"
api-name: "db_checkpoint"
source: docs/api_reference/C/db_checkpoint.html
---
## db_checkpoint

``` c
db_checkpoint [-1Vv] [-h home]
    [-k kbytes] [-L file] [-P password] [-p min]  
```

The <span class="command">**db_checkpoint**</span> utility is a daemon process that monitors the database log, and periodically calls <a href="txncheckpoint.md" class="xref" title="DB_ENV-&gt;txn_checkpoint()">DB_ENV-&gt;txn_checkpoint()</a> to checkpoint it.

The options are as follows:

- **-1**

  Force a single checkpoint of the log (regardless of whether or not there has been activity since the last checkpoint), and then exit.

  When the **-1** flag is specified, the <span class="command">**db_checkpoint**</span> utility will checkpoint the log even if unable to find an existing database environment. This functionality is useful when upgrading database environments from one version of Berkeley DB to another.

- **-h**

  Specify a home directory for the database environment; by default, the current working directory is used.

- **-k**

  Checkpoint the database at least as often as every **kbytes** of log file are written.

- **-L**

  Log the execution of the <span class="command">**db_checkpoint**</span> utility to the specified file in the following format, where <span class="emphasis">*\###*</span> is the process ID, and the date is the time the utility was started.

  ``` c
   db_checkpoint: ### Wed Jun 15 01:23:45 EDT 1995 
  ```

  This file will be removed if the <span class="command">**db_checkpoint**</span> utility exits gracefully.

- **-P**

  Specify an environment password. Although Berkeley DB utilities overwrite password strings as soon as possible, be aware there may be a window of vulnerability on systems where unprivileged users can see command-line arguments or where utilities are not able to overwrite the memory containing the command-line arguments.

- **-p**

  Checkpoint the database at least every **min** minutes if there has been any activity since the last checkpoint.

- **-V**

  Write the library version number to the standard output, and exit.

- **-v**

  Write the time of each checkpoint attempt to the standard output.

At least one of the **-1**, **-k**, and **-p** options must be specified.

The <span class="command">**db_checkpoint**</span> utility uses a Berkeley DB environment (as described for the **-h** option, the environment variable **DB_HOME**, or because the utility was run in a directory containing a Berkeley DB environment). In order to avoid environment corruption when using a Berkeley DB environment, <span class="command">**db_checkpoint**</span> should always be given the chance to detach from the environment and exit gracefully. To cause <span class="command">**db_checkpoint**</span> to release all environment resources and exit cleanly, send it an interrupt signal (SIGINT).

The <span class="command">**db_checkpoint**</span> utility does not attempt to create the Berkeley DB shared memory regions if they do not already exist. The application that creates the region should be started first, and once the region is created, the <span class="command">**db_checkpoint**</span> utility should be started.

The <a href="txncheckpoint.md" class="xref" title="DB_ENV-&gt;txn_checkpoint()">DB_ENV-&gt;txn_checkpoint()</a> method is the underlying method used by the <span class="command">**db_checkpoint**</span> utility. See the <span class="command">**db_checkpoint**</span> utility source code for an example of using `DB_ENV->txn_checkpoint()` in a IEEE/ANSI Std 1003.1 (POSIX) environment.

The <span class="command">**db_checkpoint**</span> utility exits 0 on success, and \>0 if an error occurs.

### Environment Variables

#### DB_HOME

If the **-h** option is not specified and the environment variable DB_HOME is set, it is used as the path of the database home, as described in the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method.
