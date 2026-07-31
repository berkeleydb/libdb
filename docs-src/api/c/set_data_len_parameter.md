---
title: "set_data_len"
api-name: "set_data_len"
source: docs/api_reference/C/set_data_len_parameter.html
---
## set_data_len

Limits the amount of data displayed when <a href="lockstat_print.md" class="xref" title="DB_ENV-&gt;lock_stat_print()">DB_ENV-&gt;lock_stat_print()</a> is called with the `DB_STAT_ALL` flag.

If the <a href="db_printlog.md" class="xref" title="db_printlog">db_printlog</a> or <a href="db_dump.md" class="xref" title="db_dump">db_dump</a> utility uses a `DB_CONFIG` file with this setting, it sets the the default for the amount of data displayed for each key/data item. This value may be overridden using the **-D** option for both utilities.

The value set here must be greater than 0. The default value is 100.

The syntax of this parameter in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `set_data_len`, one or more whitespace characters, and the directory name.

For example:

``` c
set_data_len 1048576
```
