---
title: "set_lk_partitions"
api-name: "set_lk_partitions"
source: docs/api_reference/C/set_lk_partitions_parameter.html
---
## set_lk_partitions

Sets the number of lock table partitions in the Berkeley DB environment. The default value is 10 times the number of CPUs on the system if there is more than one CPU.

The syntax of this parameter in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `set_lk_partitions`, one or more whitespace characters, and the number of partitions.

If the database environment already exists when this parameter is changed, it is ignored. To change this value after the environment has been created, re-create your environment.

For more information, see <a href="envset_lk_partitions.md" class="xref" title="DB_ENV-&gt;set_lk_partitions()">DB_ENV-&gt;set_lk_partitions()</a>.
