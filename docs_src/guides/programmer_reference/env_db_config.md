---
title: "DB_CONFIG configuration file"
api-name: "DB_CONFIG configuration file"
source: docs/programmer_reference/env_db_config.html
---
## DB_CONFIG configuration file

Almost all of the configuration information that can be specified to <a href="../../api/c/env.md" class="olink">DB_ENV class</a> methods can also be specified using a configuration file. If a file named DB_CONFIG exists in the database home directory, it will be read for lines of the format **NAME VALUE**.

One or more whitespace characters are used to delimit the two parts of the line, and trailing whitespace characters are discarded. All empty lines or lines whose first character is a whitespace or hash (**\#**) character will be ignored. Each line must specify both the NAME and the VALUE of the pair. The specific NAME VALUE pairs are documented in the manual for the corresponding methods (for example, the <a href="../../api/c/envset_data_dir.md" class="olink">DB_ENV-&gt;set_data_dir()</a> documentation includes NAME VALUE pair information Berkeley DB administrators can use to configure locations for database files).

The DB_CONFIG configuration file is intended to allow database environment administrators to customize environments independent of applications using the environment. For example, a database administrator can move the database log and data files to a different location without application recompilation. In addition, because the DB_CONFIG file is read when the database environment is opened, it can be used to overrule application configuration done before that time. For example a database administrator could override the compiled-in application cache size to a size more appropriate for a specific machine.
