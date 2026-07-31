---
title: "set_verbose"
api-name: "set_verbose"
source: docs/api_reference/C/set_verbose_parameter.html
---
## set_verbose

Enables/disables specific additional informational and debugging messages in the Berkeley DB message output.

The syntax of the entry in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `set_verbose`, one or more whitespace characters, the method flag parameter as a string, optionally one or more whitespace characters and the string `on` or `off`. If the optional string is omitted, the default is `on`.

For example:

``` c
set_verbose DB_VERB_RECOVERY
```

or

``` c
set_verbose DB_VERB_RECOVERY on
```

Enables display of additional information when performing recovery.

The method flag parameters are as follows:

- DB_VERB_DEADLOCK
- DB_VERB_FILEOPS
- DB_VERB_FILEOPS_ALL
- DB_VERB_RECOVERY
- DB_VERB_REGISTER
- DB_VERB_REPLICATION
- DB_VERB_REP_ELECT
- DB_VERB_REP_LEASE
- DB_VERB_REP_MISC
- DB_VERB_REP_MSGS
- DB_VERB_REP_SYNC
- DB_VERB_REP_SYSTEM
- DB_VERB_REPMGR_CONNFAIL
- DB_VERB_REPMGR_MISC
- DB_VERB_WAITSFOR

For more information, see <a href="envset_verbose.md" class="xref" title="DB_ENV-&gt;set_verbose()">DB_ENV-&gt;set_verbose()</a>.
