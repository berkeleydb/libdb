---
title: "Replication method naming"
api-name: "Replication method naming"
source: docs/upgrading/upgrade_4_5_rep_set.html
---
## Replication method naming

The method names DB_ENV-\>set_rep_limit, DB_ENV-\>get_rep_limit and DB_ENV-\>set_rep_transport have been changed to <a href="../../api/c/repset_limit.md" class="olink">DB_ENV-&gt;rep_set_limit()</a>, <a href="../../api/c/repget_limit.md" class="olink">DB_ENV-&gt;rep_get_limit()</a> and <a href="../../api/c/reptransport.md" class="olink">DB_ENV-&gt;rep_set_transport()</a> in order to be consistent with the other replication method names. That is, the characters "set_rep" and "get_rep" have been changed to "rep_set" and "rep_get".

Applications should modify the method names, no other change is required.
