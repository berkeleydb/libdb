---
title: "DB_RMW"
api-name: "DB_RMW"
source: docs/upgrading/upgrade_3_0_rmw.html
---
## DB_RMW

The following change applies only to applications using the Berkeley DB Concurrent Data Store product. If your application is not using that product, you can ignore this change.

Historically, the <a href="../../api/c/dbcursor.md" class="olink">DB-&gt;cursor()</a> method took the DB_RMW flag to indicate that the created cursor would be used for write operations on the database. This flag has been renamed to the `DB_WRITECURSOR` flag.

The application should be searched for any occurrences of DB_RMW. For each of these, any that are arguments to the <a href="../../api/c/dbcursor.md" class="olink">DB-&gt;cursor()</a> function should be changed to pass in the `DB_WRITECURSOR` flag instead.
