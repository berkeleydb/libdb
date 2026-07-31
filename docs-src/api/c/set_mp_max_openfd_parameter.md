---
title: "set_mp_max_openfd"
api-name: "set_mp_max_openfd"
source: docs/api_reference/C/set_mp_max_openfd_parameter.html
---
## set_mp_max_openfd

Limits the number of file descriptors the library will open concurrently when flushing dirty pages from the cache.

The syntax of this parameter in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `set_max_openfd`, one or more whitespace characters, and the number of open file descriptors.

For more information, see <a href="mempget_mp_max_openfd.md" class="xref" title="DB_ENV-&gt;get_mp_max_openfd()">DB_ENV-&gt;get_mp_max_openfd()</a>.
