---
title: "set_tmp_dir"
api-name: "set_tmp_dir"
source: docs/api_reference/C/set_tmp_dir_parameter.html
---
## set_tmp_dir

Specifies the path of a directory to be used as the location of temporary files. The files created to back in-memory access method databases will be created relative to this path. These temporary files can be quite large, depending on the size of the database.

The syntax of the entry in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file with the string `set_tmp_dir`, one or more whitespace characters, and the directory name.

For more information, see <a href="envset_tmp_dir.md" class="xref" title="DB_ENV-&gt;set_tmp_dir()">DB_ENV-&gt;set_tmp_dir()</a>.
