---
title: "Memory Pool API"
api-name: "Memory Pool API"
source: docs/upgrading/upgrade_4_5_memp.html
---
## Memory Pool API

As part of implementing support for multi-version concurrency control, the <a href="../../api/c/mempfget.md#fget_DB_MPOOL_DIRTY" class="olink">DB_MPOOL_DIRTY</a> flag is now specified to the <a href="../../api/c/mempfget.md" class="olink">DB_MPOOLFILE-&gt;get()</a> instead of <a href="../../api/c/mempput.md" class="olink">DB_MPOOLFILE-&gt;put()</a>, and the DB_MPOOLFILE-\>set method has been removed. In addition, a new transaction handle parameter has been added to the <a href="../../api/c/mempfget.md" class="olink">DB_MPOOLFILE-&gt;get()</a> method.

The DB_MPOOL_CLEAN flag is no longer supported.

Applications which use the memory pool API directly should update to the new API in order to use 4.5.
