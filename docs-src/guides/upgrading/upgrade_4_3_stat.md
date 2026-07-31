---
title: "DB->stat"
api-name: "DB->stat"
source: docs/upgrading/upgrade_4_3_stat.html
---
## DB-\>stat

The 4.3 release adds transactional support to the <a href="../../api/c/dbstat.md" class="olink">DB-&gt;stat()</a> method.

Application writers can simply add a NULL **txnid** argument to the <a href="../../api/c/dbstat.md" class="olink">DB-&gt;stat()</a> method calls in their application to leave the application's behavior unchanged.
