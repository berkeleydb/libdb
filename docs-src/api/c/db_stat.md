---
title: "db_stat"
api-name: "db_stat"
source: docs/api_reference/C/db_stat.html
---
## db_stat

``` c
db_stat -d file [-fN] [-h home] [-P password] [-s database]

db_stat [-cEelmNrtVxZ] [-C Aclop] [-h home] [-L A] [-M Ah] [-R A] 
        [-P password]  
```

The <span class="command">**db_stat**</span> utility displays statistics for Berkeley DB environments.

The options are as follows:

- **-C**

  Display detailed information about the locking subsystem.

  - **A**

    Display all information.

  - **c**

    Display lock conflict matrix.

  - **l**

    Display lockers within hash chains.

  - **o**

    Display lock objects within hash chains.

  - **p**

    Display locking subsystem parameters.

- **-c**

  Display locking subsystem statistics, as described in the <a href="lockstat.md" class="xref" title="DB_ENV-&gt;lock_stat()">DB_ENV-&gt;lock_stat()</a> method.

- **-d**

  Display database statistics for the specified file, as described in the <a href="dbstat.md" class="xref" title="DB-&gt;stat()">DB-&gt;stat()</a> method.

  If the database contains multiple databases and the **-s** flag is not specified, the statistics are for the internal database that describes the other databases the file contains, and not for the file as a whole.

- **-E**

  Display detailed information about the database environment.

- **-e**

  Display information about the database environment, including all configured subsystems of the database environment.

- **-f**

  Display only those database statistics that can be acquired without traversing the database.

- **-h**

  Specify a home directory for the database environment; by default, the current working directory is used.

- **-l**

  Display logging subsystem statistics, as described in the <a href="logstat.md" class="xref" title="DB_ENV-&gt;log_stat()">DB_ENV-&gt;log_stat()</a> method.

- **-L**

  Display all logging subsystem statistics.

  - **A**

    Display all information.

- **-M**

  Display detailed information about the cache.

  - **A**

    Display all information.

  - **h**

    Display buffers within hash chains.

- **-m**

  Display cache statistics, as described in the <a href="mempstat.md" class="xref" title="DB_ENV-&gt;memp_stat()">DB_ENV-&gt;memp_stat()</a> method.

- **-N**

  Do not acquire shared region mutexes while running. Other problems, such as potentially fatal errors in Berkeley DB, will be ignored as well. This option is intended only for debugging errors, and should not be used under any other circumstances.

- **-P**

  Specify an environment password. Although Berkeley DB utilities overwrite password strings as soon as possible, be aware there may be a window of vulnerability on systems where unprivileged users can see command-line arguments or where utilities are not able to overwrite the memory containing the command-line arguments.

- **-R**

  Display detailed information about the replication subsystem.

  - **A**

    Display all information.

- **-r**

  Display replication statistics, as described in in the <a href="repstat.md" class="xref" title="DB_ENV-&gt;rep_stat()">DB_ENV-&gt;rep_stat()</a> method.

- **-s**

  Display statistics for the specified database contained in the file specified with the **-d** flag.

- **-t**

  Display transaction subsystem statistics, as described in the <a href="txnstat.md" class="xref" title="DB_ENV-&gt;txn_stat()">DB_ENV-&gt;txn_stat()</a> method.

- **-V**

  Write the library version number to the standard output, and exit.

- **-x**

  Display mutex subsystem statistics, as described in the <a href="mutexstat.md" class="xref" title="DB_ENV-&gt;mutex_stat()">DB_ENV-&gt;mutex_stat()</a> method.

- **-Z**

  Reset the statistics after reporting them; valid only with the **-C**, **-c**, **-E**, **-e**, **-L**, **-l**, **-M**, **-m**, **-R**, **-r**, and **-t** options.

Values normally displayed in quantities of bytes are displayed as a combination of gigabytes (GB), megabytes (MB), kilobytes (KB), and bytes (B). Otherwise, values smaller than 10 million are displayed without any special notation, and values larger than 10 million are displayed as a number followed by "M".

The <span class="command">**db_stat**</span> utility may be used with a Berkeley DB environment (as described for the **-h** option, the environment variable **DB_HOME**, or because the utility was run in a directory containing a Berkeley DB environment). In order to avoid environment corruption when using a Berkeley DB environment, <span class="command">**db_stat**</span> should always be given the chance to detach from the environment and exit gracefully. To cause <span class="command">**db_stat**</span> to release all environment resources and exit cleanly, send it an interrupt signal (SIGINT).

The <span class="command">**db_stat**</span> utility exits 0 on success, and \>0 if an error occurs.

For information on the statistics feature for Berkeley DB SQL interface, see <a href="dbsql.md#dbsql_command_feature" class="xref" title="Command Line Features Unique to dbsql">Command Line Features Unique to dbsql</a>.

### Environment Variables

#### DB_HOME

If the **-h** option is not specified and the environment variable DB_HOME is set, it is used as the path of the database home, as described in the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method.
