---
title: "log_set_config"
api-name: "log_set_config"
source: docs/api_reference/C/log_set_config_parameter.html
---
## log_set_config

Configures the Berkeley DB logging subsystem.

The syntax of this parameter in the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `log_set_config`, one or more whitespace characters, method **flag** parameter as a string, optionally one or more whitespace characters, and the string `on` or `off`. If the optional string is omitted, the default is `on.`

The method **flag** parameters are:

- DB_LOG_DIRECT

  Turns off system buffering of Berkeley DB log files to avoid double caching.

- DB_LOG_DSYNC

  Configures Berkeley DB to flush log writes to the backing disk before returning from the write system call, rather than flushing log writes explicitly in a separate system call, as necessary.

For more information, see <a href="envlog_set_config.md" class="xref" title="DB_ENV-&gt;log_set_config()">DB_ENV-&gt;log_set_config()</a>.
