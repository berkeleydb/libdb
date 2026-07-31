---
title: "set_lk_max_objects"
api-name: "set_lk_max_objects"
source: docs/api_reference/C/set_lk_max_objects_parameter.html
---
## set_lk_max_objects

Sets the maximum number of locked objects supported by the Berkeley DB environment. This value is used to estimate how much space to allocate for various lock-table data structures. When using the DB, the default value is 1000 objects. When using the BDB SQL interface, the default value is 10,000 objects.

The syntax of this parameter in the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `set_lk_max_objects`, one or more whitespace characters, and the number of objects.

If the database environment already exists when this parameter is changed, it is ignored. To change this value after the environment has been created, re-create your environment.

For more information, see <a href="envset_lk_max_objects.md" class="xref" title="DB_ENV-&gt;set_lk_max_objects()">DB_ENV-&gt;set_lk_max_objects()</a>.
