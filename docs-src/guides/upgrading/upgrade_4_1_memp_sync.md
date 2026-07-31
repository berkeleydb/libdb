---
title: "DB_ENV->memp_sync"
api-name: "DB_ENV->memp_sync"
source: docs/upgrading/upgrade_4_1_memp_sync.html
---
## DB_ENV-\>memp_sync

Historical documentation for the <a href="../../api/c/mempsync.md" class="olink">DB_ENV-&gt;memp_sync()</a> method stated:

In addition, if <a href="../../api/c/mempsync.md" class="olink">DB_ENV-&gt;memp_sync()</a> returns success, the value of **lsn** will be overwritten with the largest log sequence number from any page that was written by <a href="../../api/c/mempsync.md" class="olink">DB_ENV-&gt;memp_sync()</a> to satisfy this request.

This functionality was never correctly implemented, and has been removed in the Berkeley DB 4.1 release. It is very unlikely application programs used this information. If your application used this information, please contact us for help in upgrading.
