---
title: "rep_set_nsites"
api-name: "rep_set_nsites"
source: docs/api_reference/C/rep_set_nsites_parameter.html
---
## rep_set_nsites

Specifies the total number of sites in a replication group. This parameter is ignored for Replication Manager applications.

The syntax of this parameter in the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `rep_set_nsites`, one or more whitespace characters, and the number of sites specified. For example:

``` c
rep_set_nsites 5
```

Sets the number of sites to 5.

For more information, see <a href="repnsites.md" class="xref" title="DB_ENV-&gt;rep_set_nsites()">DB_ENV-&gt;rep_set_nsites()</a>.
