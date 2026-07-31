---
title: "db_verify"
api-name: "db_verify"
source: docs/api_reference/C/db_verify.html
---
## db_verify

``` c
db_verify [-NoqV] [-h home] [-P password] file ...  
```

The <span class="command">**db_verify**</span> utility verifies the structure of one or more files and the databases they contain.

The options are as follows:

- **-h**

  Specify a home directory for the database environment; by default, the current working directory is used.

- **-o**

  Skip the database checks for btree and duplicate sort order and for hashing.

  If the file being verified contains databases with non-default comparison or hashing configurations, calling the <span class="command">**db_verify**</span> utility without the **-o** flag will usually return failure. The **-o** flag causes <span class="command">**db_verify**</span> to ignore database sort or hash ordering and allows <span class="command">**db_verify**</span> to be used on these files. To fully verify these files, verify them explicitly using the <a href="dbverify.md" class="xref" title="DB-&gt;verify()">DB-&gt;verify()</a> method, after configuring the correct comparison or hashing functions.

- **-N**

  Do not acquire shared region mutexes while running. Other problems, such as potentially fatal errors in Berkeley DB, will be ignored as well. This option is intended only for debugging errors, and should not be used under any other circumstances.

- **-P**

  Specify an environment password. Although Berkeley DB utilities overwrite password strings as soon as possible, be aware there may be a window of vulnerability on systems where unprivileged users can see command-line arguments or where utilities are not able to overwrite the memory containing the command-line arguments.

- **-q**

  Suppress the printing of any error descriptions, simply exit success or failure.

- **-V**

  Write the library version number to the standard output, and exit.

The <span class="command">**db_verify** utility does not perform any locking, even in Berkeley DB environments that are configured with a locking subsystem. As such, it should only be used on files that are not being modified by another thread of control.</span>

The <span class="command">**db_verify**</span> utility may be used with a Berkeley DB environment (as described for the **-h** option, the environment variable **DB_HOME**, or because the utility was run in a directory containing a Berkeley DB environment). In order to avoid environment corruption when using a Berkeley DB environment, <span class="command">**db_verify**</span> should always be given the chance to detach from the environment and exit gracefully. To cause <span class="command">**db_verify**</span> to release all environment resources and exit cleanly, send it an interrupt signal (SIGINT).

The <span class="command">**db_verify**</span> utility exits 0 on success, and \>0 if an error occurs.

### Environment Variables

#### DB_HOME

If the **-h** option is not specified and the environment variable DB_HOME is set, it is used as the path of the database home, as described in the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method.
