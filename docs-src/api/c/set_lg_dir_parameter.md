---
title: "set_lg_dir"
api-name: "set_lg_dir"
source: docs/api_reference/C/set_lg_dir_parameter.html
---
## set_lg_dir

Sets the path of the directory to be used as the location of logging files. Log files created by the Log Manager subsystem will be created in this directory. If no logging directory is specified, log files are created in the environment home directory.

The syntax of this parameter in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `set_lg_dir`, one or more whitespace characters, and the directory name.

For more information, see <a href="envset_lg_dir.md" class="xref" title="DB_ENV-&gt;set_lg_dir()">DB_ENV-&gt;set_lg_dir()</a>.
