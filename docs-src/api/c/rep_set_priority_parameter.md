---
title: "rep_set_priority"
api-name: "rep_set_priority"
source: docs/api_reference/C/rep_set_priority_parameter.html
---
## rep_set_priority

Specifies the database environment's priority in replication group elections. A special value of 0 indicates that this environment cannot be a replication group master.

The syntax of this parameter in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `rep_set_priority`, one or more whitespace characters, and the priority of this site. For example:

``` c
rep_set_priority 1
```

Sets the priority of this site to 1.

For more information, see <a href="reppriority.md" class="xref" title="DB_ENV-&gt;rep_set_priority()">DB_ENV-&gt;rep_set_priority()</a>.
