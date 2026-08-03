---
title: "DB_DEGREE_2, DB_DIRTY_READ"
api-name: "DB_DEGREE_2, DB_DIRTY_READ"
source: docs/upgrading/upgrade_4_4_isolation.html
---
## DB_DEGREE_2, DB_DIRTY_READ

The names of two isolation-level flags changed in the Berkeley DB 4.4 release. The DB_DEGREE_2 flag was renamed to <a href="../../api/c/dbcget.md#dbcget_DB_READ_COMMITTED" class="olink">DB_READ_COMMITTED</a>, and the DB_DIRTY_READ flag was renamed to <a href="../../api/c/dbopen.md#dbopen_DB_READ_UNCOMMITTED" class="olink">DB_READ_UNCOMMITTED</a>, to match ANSI standard names for isolation levels. The historic flag names continue to work in this release, but may be removed from future releases.
