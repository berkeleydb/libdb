---
title: "Replication API"
api-name: "Replication API"
source: docs/upgrading/upgrade_4_7_repapi.html
---
## Replication API

The Berkeley DB base replication API <a href="../../api/c/repelect.md" class="olink">DB_ENV-&gt;rep_elect()</a>, <a href="../../api/c/repget_nsites.md" class="olink">DB_ENV-&gt;rep_get_nsites()</a> <a href="../../api/c/repnsites.md" class="olink">DB_ENV-&gt;rep_set_nsites()</a>, <a href="../../api/c/repget_priority.md" class="olink">DB_ENV-&gt;rep_get_priority()</a> and <a href="../../api/c/reppriority.md" class="olink">DB_ENV-&gt;rep_set_priority()</a> methods now take arguments of type u_int32_t rather than int. Applications may need to change the types of arguments to these methods, or cast arguments to these methods to avoid compiler warnings.
