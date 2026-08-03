---
title: "Berkeley DB 4.4.20 Change Log"
api-name: "Berkeley DB 4.4.20 Change Log"
source: docs/upgrading/changelog_4_4_20.html
---
## Berkeley DB 4.4.20 Change Log

<span class="sect2"> [Changes since Berkeley DB 4.4.16:](changelog_4_4_20.md#idp50624312) </span>

### Changes since Berkeley DB 4.4.16:

1.  Add support for Visual Studio 2005. \[#13521\]
2.  Fix a bug with in-memory transaction logs when files wrapped around the buffer. \[#13589\]
3.  Fix a bug where we needed to close replications open files during replication initialization. \[#13623\]
4.  Fix a bug which could leave locks in the environment if database compaction was run in a transactional environment on a non-transactional database. This might have also have triggered deadlocks if the database was opened transactionally. \[#13680\]
5.  Fix a bug where setting the DB_REGISTER flag could result in unnecessarily running recovery, or corruption of the registry file on Windows systems. \[#13789\]
6.  Fix a bug in Database.compact that could cause JVM crashes or NullPointerException. \[#13791\]
7.  Fix a bug that would cause a trap if an environment was opened specifying DB_REGISTER and the environment directory could not be found. \[#13793\]
8.  Fix a buffer overflow bug when displaying process and thread IDs in the Berkeley DB statistics output. \[#13796\]
9.  Fix a bug where if there is insufficient memory for a database key in a DBT configured to return a key value into user-specified memory, the cursor is moved forward to the next entry in the database, which can cause applications to skip key/data pairs. \[#13815\]
10. Fix a bug that could cause the loss of an update to a QUEUE database in a hot backup. \[#13823\]
11. Fix a bug where retrieval from a secondary index could result in a core dump. \[#13843\]
12. Fix a bug that could cause part of the free list to become unlinked if a btree compaction was rolled back due to a transaction abort. \[#13891\]
13. Fix a bug with in-memory logging that could cause a race condition to corrupt the logs. \[#13919\]
