---
title: "DB->set_cache_priority"
api-name: "DB->set_cache_priority"
source: docs/upgrading/upgrade_4_2_priority.html
---
## DB-\>set_cache_priority

In previous releases, applications set the priority of a database's pages in the Berkeley DB buffer cache with the DB-\>set_cache_priority method. This method is no longer available. Applications wanting to set database page priorities in the buffer cache should use the <a href="../../api/c/mempset_priority.md" class="olink">mempset_priority()</a> method instead. The new call takes the same arguments and behaves identically to the old call, except that a <a href="../../api/c/memp.md" class="olink">DB_MPOOLFILE</a> buffer cache file handle is used instead of the <a href="../../api/c/db.md" class="olink">DB</a> database handle.
