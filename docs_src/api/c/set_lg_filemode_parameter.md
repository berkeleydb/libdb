---
title: "set_lg_filemode"
api-name: "set_lg_filemode"
source: docs/api_reference/C/set_lg_filemode_parameter.html
---
## set_lg_filemode

Sets the absolute file mode for created log files. This method is only useful for the rare Berkeley DB application that does not control its umask value.

Normally, if Berkeley DB applications set their umask appropriately, all processes in the application suite will have read permission on the log files created by any process in the application suite. However, if the Berkeley DB application is a library, a process using the library might set its umask to a value preventing other processes in the application suite from reading the log files it creates. In this rare case, use the set_lg_filemode parameter to set the mode of created log files to an absolute value.

The syntax of this parameter in the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `set_lg_filemode`, one or more whitespace characters, and the absolute mode of created log files.

For more information, see <a href="envset_lg_filemode.md" class="xref" title="DB_ENV-&gt;set_lg_filemode()">DB_ENV-&gt;set_lg_filemode()</a>.
