---
title: "set_create_dir"
api-name: "set_create_dir"
source: docs/api_reference/C/set_create_dir_parameter.html
---
## set_create_dir

Sets the path of a directory to be used as the location to create the access method database files.

The syntax of this parameter in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `set_create_dir`, one or more whitespace characters, and the directory name.

For example:

``` c
set_create_dir /b/data2
```

Sets data2 as the location to create the access method database files. When the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> function is used to create a file, it will be created relative to this path.

For more information, see <a href="envset_create_dir.md" class="xref" title="DB_ENV-&gt;set_create_dir()">DB_ENV-&gt;set_create_dir()</a>.
