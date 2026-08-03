---
title: "db_dump"
api-name: "db_dump"
source: docs/upgrading/upgrade_3_2_db_dump.html
---
## db_dump

In previous releases of Berkeley DB, the <a href="../../api/c/db_dump.md" class="olink">db_dump utility</a> dumped Recno access method database keys as numeric strings. For consistency, the <a href="../../api/c/db_dump.md" class="olink">db_dump utility</a> has been changed in the 3.2 release to dump record numbers as hex pairs when the data items are being dumped as hex pairs. (See the **-k** and **-p** options to the <a href="../../api/c/db_dump.md" class="olink">db_dump utility</a> for more information.) Any applications or scripts post-processing the output of the <a href="../../api/c/db_dump.md" class="olink">db_dump utility</a> for Recno databases under these conditions may require modification.
