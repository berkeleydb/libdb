---
title: "Flushing the database cache"
api-name: "Flushing the database cache"
source: docs/programmer_reference/am_sync.html
---
## Flushing the database cache

The <a href="../../api/c/dbsync.md" class="olink">DB-&gt;sync()</a> method flushes all modified records from the database cache to disk.

**It is important to understand that flushing cached information to disk only minimizes the window of opportunity for corrupted data, it does not eliminate the possibility.**

While unlikely, it is possible for database corruption to happen if a system or application crash occurs while writing data to the database. To ensure that database corruption never occurs, applications must either:

- Use transactions and logging with automatic recovery.
- Use logging and application-specific recovery.
- Edit a copy of the database, and, once all applications using the database have successfully called <a href="../../api/c/dbclose.md" class="olink">DB-&gt;close()</a>, use system operations (for example, the POSIX rename system call) to atomically replace the original database with the updated copy.
