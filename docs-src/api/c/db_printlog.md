---
title: "db_printlog"
api-name: "db_printlog"
source: docs/api_reference/C/db_printlog.html
---
## db_printlog

``` c
db_printlog [-NrV] [-b start-LSN] [-e stop-LSN] [-h home] [-P password] 
    [-D bytes] 
```

The <span class="command">**db_printlog**</span> utility is a debugging utility that dumps Berkeley DB log files in a human-readable format.

### Note

If the application(s) that use the environment make use of the <a href="envset_lg_dir.md" class="xref" title="DB_ENV-&gt;set_lg_dir()">DB_ENV-&gt;set_lg_dir()</a> method, then in order for this utility to run correctly, you need a <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file which sets the proper paths using the <a href="set_lg_dir_parameter.md" class="xref" title="set_lg_dir">set_lg_dir</a> configuration parameter.

The options are as follows:

- **-b**

  Display log records starting at log sequence number (LSN) **start-LSN**; **start-LSN** is specified as a file number, followed by a slash (/) character, followed by an offset number, with no intervening whitespace.

- **-D**

  Specifies the maximum number of bytes to display for each key/data item found in the log. This option overrides the "set_data_len" parameter found in your DB_CONFIG file, if any.

- **-e**

  Stop displaying log records at log sequence number (LSN) **stop-LSN**; **stop-LSN** is specified as a file number, followed by a slash (/) character, followed by an offset number, with no intervening whitespace.

- **-h**

  Specify a home directory for the database environment; by default, the current working directory is used.

- **-N**

  Do not acquire shared region mutexes while running. Other problems, such as potentially fatal errors in Berkeley DB, will be ignored as well. This option is intended only for debugging errors, and should not be used under any other circumstances.

- **-P**

  Specify an environment password. Although Berkeley DB utilities overwrite password strings as soon as possible, be aware there may be a window of vulnerability on systems where unprivileged users can see command-line arguments or where utilities are not able to overwrite the memory containing the command-line arguments.

- **-r**

  Read the log files in reverse order.

- **-V**

  Write the library version number to the standard output, and exit.

For more information on the <span class="command">**db_printlog**</span> output and using it to debug applications, see <a href="../../guides/installation/debug_printlog.md" class="olink">Reviewing Berkeley DB log files</a>.

The <span class="command">**db_printlog**</span> utility uses a Berkeley DB environment (as described for the **-h** option, the environment variable **DB_HOME**, or because the utility was run in a directory containing a Berkeley DB environment). In order to avoid environment corruption when using a Berkeley DB environment, <span class="command">**db_printlog**</span> should always be given the chance to detach from the environment and exit gracefully. To cause <span class="command">**db_printlog**</span> to release all environment resources and exit cleanly, send it an interrupt signal (SIGINT).

The <span class="command">**db_printlog**</span> utility exits 0 on success, and \>0 if an error occurs.

### Environment Variables

#### DB_HOME

If the **-h** option is not specified and the environment variable DB_HOME is set, it is used as the path of the database home, as described in the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method.
