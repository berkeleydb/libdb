---
title: "DB_CHECKPOINT, DB_CURLSN"
api-name: "DB_CHECKPOINT, DB_CURLSN"
source: docs/upgrading/upgrade_4_1_checkpoint.html
---
## DB_CHECKPOINT, DB_CURLSN

The DB_CHECKPOINT flag has been removed from the <a href="../../api/c/logcget.md" class="olink">DB_LOGC-&gt;get()</a> and <a href="../../api/c/logput.md" class="olink">DB_ENV-&gt;log_put()</a> methods. It is very unlikely application programs used this flag. If your application used this flag, please contact us for help in upgrading.

The DB_CURLSN flag has been removed from the <a href="../../api/c/logput.md" class="olink">DB_ENV-&gt;log_put()</a> method. It is very unlikely application programs used this flag. If your application used this flag, please contact us for help in upgrading.
