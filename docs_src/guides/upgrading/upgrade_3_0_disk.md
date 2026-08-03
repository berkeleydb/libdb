---
title: "Upgrade Requirements"
api-name: "Upgrade Requirements"
source: docs/upgrading/upgrade_3_0_disk.html
---
## Upgrade Requirements

Log file formats and the Btree, Recno and Hash Access Method database formats changed in the Berkeley DB 3.0 release. (The on-disk Btree/Recno format changed from version 6 to version 7. The on-disk Hash format changed from version 5 to version 6.) Until the underlying databases are upgraded, the <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a> method will return a `DB_OLD_VERSION` error.

For further information on upgrading Berkeley DB installations, see <a href="upgrade_process.md" class="xref" title="Chapter 2.  Upgrading from previous versions of Berkeley DB">Upgrading from previous versions of Berkeley DB</a> .
