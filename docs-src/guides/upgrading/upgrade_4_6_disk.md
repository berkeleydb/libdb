---
title: "Upgrade Requirements"
api-name: "Upgrade Requirements"
source: docs/upgrading/upgrade_4_6_disk.html
---
## Upgrade Requirements

The log file format changed in the Berkeley DB 4.6 release.

The format of Hash database pages was changed in the Berkeley DB 4.6 release, and items are now stored in sorted order. **The format changes are entirely backward-compatible, and no database upgrades are needed.** However, upgrading existing databases can offer significant performance improvements. Note that databases created using the 4.6 release may not be usable with earlier Berkeley DB releases.

For further information on upgrading Berkeley DB installations, see <a href="upgrade_process.md" class="xref" title="Chapter 2.  Upgrading from previous versions of Berkeley DB">Upgrading from previous versions of Berkeley DB</a> .
