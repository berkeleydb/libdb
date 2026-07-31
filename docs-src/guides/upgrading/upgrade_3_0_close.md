---
title: "DB->sync and DB->close"
api-name: "DB->sync and DB->close"
source: docs/upgrading/upgrade_3_0_close.html
---
## DB-\>sync and DB-\>close

In previous Berkeley DB releases, the <a href="../../api/c/dbclose.md" class="olink">DB-&gt;close()</a> and <a href="../../api/c/dbsync.md" class="olink">DB-&gt;sync()</a> methods discarded any return of DB_INCOMPLETE from the underlying buffer pool interfaces, and returned success to its caller. (The DB_INCOMPLETE error will be returned if the buffer pool functions are unable to flush all of the database's dirty blocks from the pool. This often happens if another thread is reading or writing the database's pages in the pool.)

In the 3.X release, <a href="../../api/c/dbsync.md" class="olink">DB-&gt;sync()</a> and <a href="../../api/c/dbclose.md" class="olink">DB-&gt;close()</a> will return DB_INCOMPLETE to the application. The best solution is to not call <a href="../../api/c/dbsync.md" class="olink">DB-&gt;sync()</a> with the <a href="../../api/c/dbclose.md#dbclose_DB_NOSYNC" class="olink">DB_NOSYNC</a> flag to the <a href="../../api/c/dbclose.md" class="olink">DB-&gt;close()</a> method when multiple threads are expected to be accessing the database. Alternatively, the caller can ignore any error return of DB_INCOMPLETE.
