---
title: "db_replicate"
api-name: "db_replicate"
source: docs/api_reference/C/db_replicate.html
---
## db_replicate

``` c
db_replicate [-MVv] [-h home]
    [-L file] [-P password] [-T num_threads] [-t secs]  
```

The <span class="command">**db_replicate**</span> utility is a daemon process that provides replication/HA services on a transactional environment. This utility enables you to upgrade an existing Transactional Data Store application to an HA application with minor modifications. For more information on the db_replicate utility, see the <a href="../../programmer_reference/rep_replicate.html" class="olink">Running Replication Using the db_replicate Utility</a> section in the <span class="emphasis">*Berkeley DB Programmer's Reference Guide.*</span>

### Note

This utility is not supported for use with the DB SQL APIs.

The options are as follows:

- **-h**

  Specify a home directory for the database environment; by default, the current working directory is used.

- **-L**

  Log the execution of the <span class="command">**db_replicate**</span> utility to the specified file in the following format, where <span class="emphasis">*\###*</span> is the process ID, and the date is the time the utility was started.

  ``` c
   db_replicate: ### Wed Jun 15 01:23:45 EDT 1995 
  ```

  Additionally, events such as site role changes will be noted in the log file. This file will be removed if the <span class="command">**db_replicate**</span> utility exits gracefully.

- **-M**

  Start the <span class="command">**db_replicate**</span> utility to be the master site of the replication group. Otherwise, the site will be started as a client replica.

- **-P**

  Specify an environment password. Although Berkeley DB utilities overwrite password strings as soon as possible, be aware there may be a window of vulnerability on systems where unprivileged users can see command-line arguments or where utilities are not able to overwrite the memory containing the command-line arguments.

- **-T**

  Specify the number of replication message processing threads.

- **-t**

  Specify how often (in seconds) the utility will check for program interruption and resend the last log record.

- **-V**

  Write the library version number to the standard output, and exit.

- **-v**

  Turn on replication verbose messages. These messages will be written to the standard output and will be quite voluminous.

The <span class="command">**db_replicate**</span> utility uses a Berkeley DB environment (as described for the **-h** option, the environment variable **DB_HOME**, or because the utility was run in a directory containing a Berkeley DB environment). In order to avoid environment corruption when using a Berkeley DB environment, <span class="command">**db_replicate**</span> should always be given the chance to detach from the environment and exit gracefully. To cause <span class="command">**db_replicate**</span> to release all environment resources and exit cleanly, send it an interrupt signal (SIGINT).

The <span class="command">**db_replicate**</span> utility does not attempt to create the Berkeley DB shared memory regions if they do not already exist. The application that creates the region should be started first, and once the region is created, the <span class="command">**db_replicate**</span> utility should be started. The application must use the <a href="envopen.md#envopen_DB_INIT_REP" class="xref"><code class="literal">DB_INIT_REP</code></a> and <a href="envopen.md#envopen_DB_THREAD" class="xref"><code class="literal">DB_THREAD</code></a> flags when creating the environment.

The <span class="command">**db_replicate**</span> utility exits 0 on success, and \>0 if an error occurs.

### Environment Variables

#### DB_HOME

If the **-h** option is not specified and the environment variable DB_HOME is set, it is used as the path of the database home, as described in the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method.
