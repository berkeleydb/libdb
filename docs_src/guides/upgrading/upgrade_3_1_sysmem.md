---
title: "DB_SYSTEM_MEM"
api-name: "DB_SYSTEM_MEM"
source: docs/upgrading/upgrade_3_1_sysmem.html
---
## DB_SYSTEM_MEM

Using the <a href="../../api/c/envopen.md#envopen_DB_SYSTEM_MEM" class="olink">DB_SYSTEM_MEM</a> option on UNIX systems now requires the specification of a base system memory segment ID, using the <a href="../../api/c/envset_shm_key.md" class="olink">DB_ENV-&gt;set_shm_key()</a> method. Any valid segment ID may be specified, for example, one returned by the UNIX `ftok`(3) function.
