---
title: "set_lk_max_lockers"
api-name: "set_lk_max_lockers"
source: docs/api_reference/C/set_lk_max_lockers_parameter.html
---
## set_lk_max_lockers

Sets the maximum number of locking entities supported by the Berkeley DB environment. This value is used by Berkeley DB to estimate how much space to allocate for various lock-table data structures. When using the DB, the default value is 1,000 lockers. When using the BDB SQL interface, the default value is 2,000 lockers.

The syntax of this parameter in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `set_lk_max_lockers`, one or more whitespace characters, and the number of lockers.

If the database environment already exists when this parameter is changed, it is ignored. To change this value after the environment has been created, re-create your environment.

For more information, see <a href="envset_lk_max_lockers.md" class="xref" title="DB_ENV-&gt;set_lk_max_lockers()">DB_ENV-&gt;set_lk_max_lockers()</a>.
