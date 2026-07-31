---
title: "DB_ENV->rep_elect"
api-name: "DB_ENV->rep_elect"
source: docs/upgrading/upgrade_4_5_elect.html
---
## DB_ENV-\>rep_elect

Two of the historic arguments for the <a href="../../api/c/repelect.md" class="olink">DB_ENV-&gt;rep_elect()</a> method have been moved from the interface to separate methods in order to make them available within the new replication manager framework.

The **priority** parameter should now be explicitly set using the <a href="../../api/c/reppriority.md" class="olink">DB_ENV-&gt;rep_set_priority()</a> method. To upgrade existing replication applications to the Berkeley DB 4.5 <a href="../../api/c/repelect.md" class="olink">DB_ENV-&gt;rep_elect()</a> interface, it may be simplest to insert a call to <a href="../../api/c/reppriority.md" class="olink">DB_ENV-&gt;rep_set_priority()</a> immediately before the existing call to <a href="../../api/c/repelect.md" class="olink">DB_ENV-&gt;rep_elect()</a>. Alternatively, it may make more sense to add a single call to <a href="../../api/c/reppriority.md" class="olink">DB_ENV-&gt;rep_set_priority()</a> during database environment configuration.

The **timeout** parameter should now be explicitly set using the <a href="../../api/c/repset_timeout.md" class="olink">DB_ENV-&gt;rep_set_timeout()</a> method. To upgrade existing replication applications to the Berkeley DB 4.5 <a href="../../api/c/repelect.md" class="olink">DB_ENV-&gt;rep_elect()</a> interface, it may be simplest to insert a call to <a href="../../api/c/repset_timeout.md" class="olink">DB_ENV-&gt;rep_set_timeout()</a> immediately before the existing call to <a href="../../api/c/repelect.md" class="olink">DB_ENV-&gt;rep_elect()</a>. Alternatively, it may make more sense to add a single call to <a href="../../api/c/repset_timeout.md" class="olink">DB_ENV-&gt;rep_set_timeout()</a> during database environment configuration.
