---
title: "DB_INCOMPLETE"
api-name: "DB_INCOMPLETE"
source: docs/upgrading/upgrade_3_2_incomplete.html
---
## DB_INCOMPLETE

There are a number of functions that flush pages from the Berkeley DB shared memory buffer pool to disk. Most of those functions can potentially fail because a page that needs to be flushed is not currently available. However, this is not a hard failure and is rarely cause for concern. In the Berkeley DB 3.2 release, the C++ API (if that API is configured to throw exceptions) and the Java API have been changed so that this failure does not throw an exception, but rather returns a non-zero error code of `DB_INCOMPLETE`.

The following C++ methods will return `DB_INCOMPLETE` rather than throw an exception: <a href="../api_reference/CXX/dbclose.html" class="olink">Db::close</a>, <a href="../api_reference/CXX/dbsync.html" class="olink">Db::sync</a>, <a href="../api_reference/CXX/mempsync.html" class="olink">DbEnv::memp_sync</a>, <a href="../api_reference/CXX/txncheckpoint.html" class="olink">DbEnv::txn_checkpoint</a>, and <a href="../api_reference/CXX/mempfsync.html" class="olink">DbMpoolFile::memp_fsync</a>.

The following Java methods are now declared "public int" rather than "public void", and will return `Db.DB_INCOMPLETE` rather than throw an exception: `Db.close()`, `Db.sync()`, and `DbEnv.checkpoint()`.

It is likely that the only change required by any application will be those currently checking for a `DB_INCOMPLETE` return that has been encapsulated in an exception.
