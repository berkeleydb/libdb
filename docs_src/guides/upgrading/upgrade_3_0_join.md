---
title: "DB->join"
api-name: "DB->join"
source: docs/upgrading/upgrade_3_0_join.html
---
## DB-\>join

Historically, the last two arguments to the <a href="../../api/c/dbjoin.md" class="olink">DB-&gt;join()</a> method were a flags value followed by a reference to a memory location to store the returned cursor object. In the Berkeley DB 3.0 release, the order of those two arguments has been swapped for consistency with other Berkeley DB interfaces.

The application should be searched for any occurrences of <a href="../../api/c/dbjoin.md" class="olink">DB-&gt;join()</a>. For each of these, the order of the last two arguments should be swapped.
