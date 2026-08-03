---
title: "DB_ENV->set_lk_max"
api-name: "DB_ENV->set_lk_max"
source: docs/upgrading/upgrade_4_0_set_lk_max.html
---
## DB_ENV-\>set_lk_max

The DB_ENV-\>set_lk_max method has been deprecated in favor of the <a href="../../api/c/envset_lk_max_locks.md" class="olink">DB_ENV-&gt;set_lk_max_locks()</a>, <a href="../../api/c/envset_lk_max_lockers.md" class="olink">DB_ENV-&gt;set_lk_max_lockers()</a>, and <a href="../../api/c/envset_lk_max_objects.md" class="olink">DB_ENV-&gt;set_lk_max_objects()</a> methods. The DB_ENV-\>set_lk_max method continues to be available, but is no longer documented and is expected to be removed in a future release.
