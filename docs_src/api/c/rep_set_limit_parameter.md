---
title: "rep_set_limit"
api-name: "rep_set_limit"
source: docs/api_reference/C/rep_set_limit_parameter.html
---
## rep_set_limit

Sets record transmission throttling. This is a bytecount limit on the amount of data that will be transmitted from a site in response to a single message processed by the `DB_ENV->rep_process_message` method.

The syntax of this parameter in the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `rep_set_limit`, one or more whitespace characters, and the limit specified in two parts: the gigabytes and the bytes values. For example:

``` c
rep_set_limit 0 1048576
```

Sets a 1 megabyte limit.

For more information, see <a href="repset_limit.md" class="xref" title="DB_ENV-&gt;rep_set_limit()">DB_ENV-&gt;rep_set_limit()</a>.
