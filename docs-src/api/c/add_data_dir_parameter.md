---
title: "add_data_dir"
api-name: "add_data_dir"
source: docs/api_reference/C/add_data_dir_parameter.html
---
## add_data_dir

Add the path of a directory to be used as the location of the access method database files. Paths specified to the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> function will be searched relative to this path. Paths set using this method are additive, and specifying more than one will result in each specified directory being searched for database files.

The syntax of this parameter in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `add_data_dir`, one or more whitespace characters, and the directory name.

For more information, see <a href="envadd_data_dir.md" class="xref" title="DB_ENV-&gt;add_data_dir()">DB_ENV-&gt;add_data_dir()</a>.
