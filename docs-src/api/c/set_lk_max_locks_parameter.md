---
title: "set_lk_max_locks"
api-name: "set_lk_max_locks"
source: docs/api_reference/C/set_lk_max_locks_parameter.html
---
## set_lk_max_locks

Sets the maximum number of locks supported by the Berkeley DB environment. This value is used to estimate how much space to allocate for various lock-table data structures. When using the DB, the default value is 1000 locks. When using the BDB SQL interface, the default value is 10,000 locks.

The syntax of this parameter in the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `set_lk_max_locks`, one or more whitespace characters, and the number of locks.

If the database environment already exists when this parameter is changed, it is ignored. To change this value after the environment has been created, re-create your environment.

For more information, see <a href="envset_lk_max_locks.md" class="xref" title="DB_ENV-&gt;set_lk_max_locks()">DB_ENV-&gt;set_lk_max_locks()</a>.
