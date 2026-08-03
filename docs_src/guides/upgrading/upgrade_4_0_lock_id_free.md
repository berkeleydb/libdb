---
title: "DB_ENV->lock_id_free"
api-name: "DB_ENV->lock_id_free"
source: docs/upgrading/upgrade_4_0_lock_id_free.html
---
## DB_ENV-\>lock_id_free

A new locker ID related API, the <a href="../../api/c/lockid_free.md" class="olink">DB_ENV-&gt;lock_id_free()</a> method, was added to Berkeley DB 4.0 release. Applications using the <a href="../../api/c/lockid.md" class="olink">DB_ENV-&gt;lock_id()</a> method to allocate locker IDs may want to update their applications to free the locker ID when it is no longer needed.
