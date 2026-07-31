---
title: "set_lg_regionmax"
api-name: "set_lg_regionmax"
source: docs/api_reference/C/set_lg_regionmax_parameter.html
---
## set_lg_regionmax

Sets the size of the underlying logging area of the Berkeley DB environment, in bytes. By default, or if the value is set to 0, the minimum region size is used, approximately 128KB. The log region is used to store filenames, and so may need to be increased in size if a large number of files will be opened and registered with the specified Berkeley DB environment's log manager.

The syntax of this parameter in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `set_lg_regionmax`, one or more whitespace characters, and the log region size in bytes.

If the database environment already exists when this parameter is changed, it is ignored. To change this value after the environment has been created, re-create your environment.

For more information, see <a href="envget_lg_regionmax.md" class="xref" title="DB_ENV-&gt;get_lg_regionmax()">DB_ENV-&gt;get_lg_regionmax()</a>.
