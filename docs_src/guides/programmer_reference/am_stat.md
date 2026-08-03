---
title: "Database statistics"
api-name: "Database statistics"
source: docs/programmer_reference/am_stat.html
---
## Database statistics

The <a href="../../api/c/dbstat.md" class="olink">DB-&gt;stat()</a> method returns a set of statistics about the underlying database, for example, the number of key/data pairs in the database, how the database was originally configured, and so on.

There is a flag you can set to avoid time-consuming operations:

<span class="term"> <a href="../../api/c/dbstat.md#stat_DB_FAST_STAT" class="olink">DB_FAST_STAT</a> </span>  
Return only information that can be acquired without traversing the entire database.
