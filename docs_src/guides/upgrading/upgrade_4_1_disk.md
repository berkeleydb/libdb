---
title: "Upgrade Requirements"
api-name: "Upgrade Requirements"
source: docs/upgrading/upgrade_4_1_disk.html
---
## Upgrade Requirements

The log file format changed in the Berkeley DB 4.1 release.

All of the access method database formats changed in the Berkeley DB 4.1 release (Btree/Recno: version 8 to version 9, Hash: version 7 to version 8, and Queue: version 3 to version 4). **The format changes are entirely backward-compatible, and no database upgrades are needed.** Note that databases created using the 4.1 release may not be usable with earlier Berkeley DB releases.

For further information on upgrading Berkeley DB installations, see <a href="upgrade_process.md" class="xref" title="Chapter 2.  Upgrading from previous versions of Berkeley DB">Upgrading from previous versions of Berkeley DB</a> .
