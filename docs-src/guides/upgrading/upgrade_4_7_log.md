---
title: "Log configuration"
api-name: "Log configuration"
source: docs/upgrading/upgrade_4_7_log.html
---
## Log configuration

In the Berkeley DB 4.7 release, the logging subsystem is configured using the <a href="../../api/c/envlog_set_config.md" class="olink">DB_ENV-&gt;log_set_config()</a> method instead of the previously used <a href="../../api/c/envset_flags.md" class="olink">DB_ENV-&gt;set_flags()</a> method.

The <a href="../../api/c/envset_flags.md" class="olink">DB_ENV-&gt;set_flags()</a> method no longer accepts the flags DB_DIRECT_LOG, DB_DSYNC_LOG, DB_LOG_INMEMORY or DB_LOG_AUTOREMOVE. Applications should be modified to use the equivalent flags accepted by the <a href="../../api/c/envlog_set_config.md" class="olink">DB_ENV-&gt;log_set_config()</a> method.

| Previous <a href="../../api/c/envset_flags.md" class="olink">DB_ENV-&gt;set_flags()</a> flag | Replacement <a href="../../api/c/envlog_set_config.md" class="olink">DB_ENV-&gt;log_set_config()</a> flag |
|----|----|
| DB_DIRECT_LOG | DB_LOG_DIRECT |
| DB_DSYNC_LOG | DB_LOG_DSYNC |
| DB_LOG_INMEMORY | DB_LOG_IN_MEMORY |
| DB_LOG_AUTOREMOVE | DB_LOG_AUTO_REMOVE |
