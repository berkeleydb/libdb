---
title: "rep_set_config"
api-name: "rep_set_config"
source: docs/api_reference/C/rep_set_config_parameter.html
---
## rep_set_config

Configures the Berkeley DB replication subsystem.

The syntax of this parameter in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `rep_set_config`, one or more whitespace characters, and the method parameter as a string and optionally one or more whitespace characters, and the string `on` or `off`. If the optional string is omitted, the default is `on`. For example:

``` c
rep_set_config DB_REP_CONF_NOWAIT on
```

or

``` c
rep_set_config DB_REP_CONF_NOWAIT
```

Configures the Berkeley DB replication subsystem such that the method calls that would normally block while clients are in recovery will return errors immediately.

The method parameters are:

- DB_REP_CONF_AUTOINIT
- DB_REP_CONF_BULK
- DB_REP_CONF_DELAYCLIENT
- DB_REP_CONF_INMEM
- DB_REP_CONF_LEASE
- DB_REP_CONF_NOWAIT
- DB_REPMGR_CONF_ELECTIONS
- DB_REPMGR_CONF_2SITE_STRICT

For more information, see <a href="repconfig.md" class="xref" title="DB_ENV-&gt;rep_set_config()">DB_ENV-&gt;rep_set_config()</a>.
