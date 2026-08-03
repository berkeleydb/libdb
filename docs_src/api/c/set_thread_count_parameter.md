---
title: "set_thread_count"
api-name: "set_thread_count"
source: docs/api_reference/C/set_thread_count_parameter.html
---
## set_thread_count

Declares an approximate number of threads in the database environment.

The syntax of the entry in in the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `set_thread_count`, one or more whitespace characters, and the thread count. The DB_CONFIG file is read when the database environment is opened, and hence it silently overrules configuration done before that time.

For more information, see <a href="envset_thread_count.md" class="xref" title="DB_ENV-&gt;set_thread_count()">DB_ENV-&gt;set_thread_count()</a>.
