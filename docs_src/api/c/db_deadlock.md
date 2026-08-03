---
title: "db_deadlock"
api-name: "db_deadlock"
source: docs/api_reference/C/db_deadlock.html
---
## db_deadlock

``` c
db_deadlock [-Vv]
    [-a e | m | n | o | W | w | y] [-h home] [-L file] [-t sec.usec]  
```

The <span class="command">**db_deadlock**</span> utility traverses the database environment lock region, and aborts a lock request each time it detects a deadlock or a lock request that has timed out. By default, in the case of a deadlock, a random lock request is chosen to be aborted.

This utility should be run as a background daemon, or the underlying Berkeley DB deadlock detection interfaces should be called in some other way, whenever there are multiple threads or processes accessing a database and at least one of them is modifying it.

The options are as follows:

- **-a**

  When a deadlock is detected, abort the locker:

  - **m**

    with the most locks

  - **n**

    with the fewest locks

  - **o**

    with the oldest locks

  - **W**

    with the most write locks

  - **w**

    with the fewest write locks

  - **y**

    with the youngest locks

  - **e**

    When lock or transaction timeouts have been specified, abort any lock request that has timed out. Note that this option does not perform the entire deadlock detection algorithm, but instead only checks for timeouts.

- **-h**

  Specify a home directory for the database environment; by default, the current working directory is used.

- **-L**

  Log the execution of the <span class="command">**db_deadlock**</span> utility to the specified file in the following format, where <span class="emphasis">*\###*</span> is the process ID, and the date is the time the utility was started.

  ``` c
           db_deadlock: ### Wed Jun 15 01:23:45 EDT 1995 
  ```

  This file will be removed if the <span class="command">**db_deadlock**</span> utility exits gracefully.

- **-t**

  Check the database environment every **sec** seconds plus **usec** microseconds to see if a process has been forced to wait for a lock; if one has, review the database environment lock structures.

- **-V**

  Write the library version number to the standard output, and exit.

- **-v**

  Run in verbose mode, generating messages each time the detector runs.

If the **-t** option is not specified, <span class="command">**db_deadlock**</span> will run once and exit.

The <span class="command">**db_deadlock**</span> utility uses a Berkeley DB environment (as described for the **-h** option, the environment variable **DB_HOME**, or because the utility was run in a directory containing a Berkeley DB environment). In order to avoid environment corruption when using a Berkeley DB environment, <span class="command">**db_deadlock**</span> should always be given the chance to detach from the environment and exit gracefully. To cause <span class="command">**db_deadlock**</span> to release all environment resources and exit cleanly, send it an interrupt signal (SIGINT).

The <span class="command">**db_deadlock**</span> utility does not attempt to create the Berkeley DB shared memory regions if they do not already exist. The application which creates the region should be started first, and then, once the region is created, the <span class="command">**db_deadlock**</span> utility should be started.

The <a href="lockdetect.md" class="xref" title="DB_ENV-&gt;lock_detect()">DB_ENV-&gt;lock_detect()</a> method is the underlying method used by the <span class="command">**db_deadlock**</span> utility. See the <span class="command">**db_deadlock**</span> utility source code for an example of using `DB_ENV->lock_detect()` in a IEEE/ANSI Std 1003.1 (POSIX) environment.

The <span class="command">**db_deadlock**</span> utility exits 0 on success, and \>0 if an error occurs.

### Environment Variables

#### DB_HOME

If the **-h** option is not specified and the environment variable DB_HOME is set, it is used as the path of the database home, as described in the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method.
