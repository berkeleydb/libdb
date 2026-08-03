---
title: "deprecated interfaces"
api-name: "deprecated interfaces"
source: docs/upgrading/upgrade_4_5_deprecate.html
---
## deprecated interfaces

Some previously deprecated interfaces were removed from the Berkeley DB 4.5 release:

- The DB_ENV-\>set_lk_max method was removed. This method has been deprecated and undocumented since the Berkeley DB 4.0 release.
- The DB-\>stat method flags DB_CACHED_COUNT and DB_RECORDCOUNT were removed. These flags have been deprecated and undocumented since the Berkeley DB 4.1 release.
- The **-w** option to the <a href="../../api/c/db_deadlock.md" class="olink">db_deadlock utility</a> was removed. This option has been deprecated and undocumented since the Berkeley DB 4.0 release.
