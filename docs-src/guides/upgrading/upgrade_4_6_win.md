---
title: "Windows 9X"
api-name: "Windows 9X"
source: docs/upgrading/upgrade_4_6_win.html
---
## Windows 9X

Berkeley DB no longer supports process-shared database environments on Windows 9X platforms; the <a href="../../api/c/envopen.md#envopen_DB_PRIVATE" class="olink">DB_PRIVATE</a> flag must always be specified to the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> method.
