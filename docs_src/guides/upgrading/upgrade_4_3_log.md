---
title: "Logging"
api-name: "Logging"
source: docs/upgrading/upgrade_4_3_log.html
---
## Logging

In previous releases, the <a href="../../api/c/envset_flags.md" class="olink">DB_ENV-&gt;set_flags()</a> method flag DB_TXN_NOT_DURABLE specified that transactions for the entire database environment were not durable. However, it was not possible to set this flag in environments that were part of replication groups, and physical log files were still created. The 4.3 release adds support for true in-memory logging for both replication and non-replicated sites.

Existing applications setting the DB_TXN_NOT_DURABLE flag for database environments should be upgraded to set the DB_LOG_INMEMORY flag instead.

In previous releases, log buffer sizes were restricted to be less than or equal to the log file size; this restriction is no longer required.
