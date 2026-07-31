---
title: "st_flushcommit"
api-name: "st_flushcommit"
source: docs/upgrading/upgrade_4_1_log_stat.html
---
## st_flushcommit

The DB_ENV-\>log_stat "st_flushcommits" statistic has been removed from Berkeley DB, as it is now the same as the "st_scount" statistic. Any application using the "st_flushcommits" statistic should remove it, or replace it with the "st_count" statistic.
