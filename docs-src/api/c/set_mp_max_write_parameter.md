---
title: "set_mp_max_write"
api-name: "set_mp_max_write"
source: docs/api_reference/C/set_mp_max_write_parameter.html
---
## set_mp_max_write

Limits the number of sequential write operations scheduled by the library when flushing dirty pages from the cache.

The syntax of this parameter in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `set_mp_max_write`, one or more whitespace characters, and the maximum number of sequential writes and the number of microseconds to sleep, also separated by whitespace characters.

For more information, see <a href="mempset_mp_max_write.md" class="xref" title="DB_ENV-&gt;set_mp_max_write()">DB_ENV-&gt;set_mp_max_write()</a>.
