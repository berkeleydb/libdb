---
title: "Upgrade Requirements"
api-name: "Upgrade Requirements"
source: docs/upgrading/upgrade_3_1_disk.html
---
## Upgrade Requirements

Log file formats and the Btree, Queue, Recno and Hash Access Method database formats changed in the Berkeley DB 3.1 release. (The on-disk Btree/Recno format changed from version 7 to version 8. The on-disk Hash format changed from version 6 to version 7. The on-disk Queue format changed from version 1 to version 2.) Until the underlying databases are upgraded, the <a href="../../api/c/dbopen.md" class="olink">DB-&gt;open()</a> method will return a `DB_OLD_VERSION` error.

An additional flag, <a href="../../api/c/dbset_flags.md#dbset_flags_DB_DUPSORT" class="olink">DB_DUPSORT</a>, has been added to the <a href="../../api/c/dbupgrade.md" class="olink">DB-&gt;upgrade()</a> method for this upgrade. Please review the <a href="../../api/c/dbupgrade.md" class="olink">DB-&gt;upgrade()</a> documentation for further information.

For further information on upgrading Berkeley DB installations, see <a href="upgrade_process.md" class="xref" title="Chapter 2.  Upgrading from previous versions of Berkeley DB">Upgrading from previous versions of Berkeley DB</a> .
