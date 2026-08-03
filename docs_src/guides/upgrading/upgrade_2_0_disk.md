---
title: "Upgrade Requirements"
api-name: "Upgrade Requirements"
source: docs/upgrading/upgrade_2_0_disk.html
---
## Upgrade Requirements

You will need to upgrade your on-disk databases, as all access method database formats changed in the Berkeley DB 2.0 release. For information on converting databases from Berkeley DB 1.85 to Berkeley DB 2.0, see the <a href="../../api/c/db_dump.md" class="olink">db_dump185 utility</a> and <a href="../../api/c/db_load.md" class="olink">db_load utility</a> documentation. As database environments did not exist prior to the 2.0 release, there is no question of upgrading existing database environments.
