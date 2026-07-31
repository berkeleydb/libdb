---
title: "SunOS"
api-name: "SunOS"
source: docs/installation/build_unix_sunos.html
---
## SunOS

1.  **I can't specify the <a href="../../api/c/envopen.md#envopen_DB_SYSTEM_MEM" class="olink">DB_SYSTEM_MEM</a> flag to <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a>.**

    The `shmget`(2) interfaces are not used on SunOS releases prior to 5.0, even though they apparently exist, because the distributed include files did not allow them to be compiled. For this reason, it will not be possible to specify the <a href="../../api/c/envopen.md#envopen_DB_SYSTEM_MEM" class="olink">DB_SYSTEM_MEM</a> flag to those versions of SunOS.
