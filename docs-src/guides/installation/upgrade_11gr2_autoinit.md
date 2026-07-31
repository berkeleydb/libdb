---
title: "DB_REP_CONF_NOAUTOINIT Replaced"
api-name: "DB_REP_CONF_NOAUTOINIT Replaced"
source: docs/installation/upgrade_11gr2_autoinit.html
---
## DB_REP_CONF_NOAUTOINIT Replaced

In this release, the `DB_REP_CONF_NOAUTOINIT` flag is replaced by the `DB_REP_CONF_AUTOINIT` flag. This option is ON by default. To turn off automatic internal initialization, call the `DB_ENV->rep_set_config` method with the **which** parameter set to `DB_REP_CONF_AUTOINIT` and the **onoff** parameter set to zero.
