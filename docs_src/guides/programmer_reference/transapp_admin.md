---
title: "Environment infrastructure"
api-name: "Environment infrastructure"
source: docs/programmer_reference/transapp_admin.html
---
## Environment infrastructure

When building transactional applications, it is usually necessary to build an administrative infrastructure around the database environment. There are five components to this infrastructure, and each is supported by the Berkeley DB package in two different ways: a standalone utility and one or more library interfaces.

- Deadlock detection: <a href="../../api/c/db_deadlock.md" class="olink">db_deadlock</a> utility, <a href="../../api/c/lockdetect.md" class="olink">DB_ENV-&gt;lock_detect()</a>, <a href="../../api/c/envset_lk_detect.md" class="olink">DB_ENV-&gt;set_lk_detect()</a>
- Checkpoints: the <a href="../../api/c/db_checkpoint.md" class="olink">db_checkpoint</a> utility, <a href="../../api/c/txncheckpoint.md" class="olink">DB_ENV-&gt;txn_checkpoint()</a>
- Database and log file archival: the <a href="../../api/c/db_archive.md" class="olink">db_archive</a> utility, <a href="../../api/c/logarchive.md" class="olink">DB_ENV-&gt;log_archive()</a>
- Log file removal: <a href="../../api/c/db_archive.md" class="olink">db_archive</a> utility, <a href="../../api/c/logarchive.md" class="olink">DB_ENV-&gt;log_archive()</a>
- Recovery procedures: <a href="../../api/c/db_recover.md" class="olink">db_recover</a> utility, <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a>

When writing multithreaded server applications and/or applications intended for download from the Web, it is usually simpler to create local threads that are responsible for administration of the database environment as scheduling is often simpler in a single-process model, and only a single binary need be installed and run. However, the supplied utilities can be generally useful tools even when the application is responsible for doing its own administration because applications rarely offer external interfaces to database administration. The utilities are required when programming to a Berkeley DB scripting interface because the scripting APIs do not always offer interfaces to the administrative functionality.
