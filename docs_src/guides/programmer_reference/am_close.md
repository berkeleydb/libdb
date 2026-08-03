---
title: "Database close"
api-name: "Database close"
source: docs/programmer_reference/am_close.html
---
## Database close

The <a href="../../api/c/dbclose.md" class="olink">DB-&gt;close()</a> database handle closes the <a href="../../api/c/db.md" class="olink">DB</a> handle. By default, <a href="../../api/c/dbclose.md" class="olink">DB-&gt;close()</a> also flushes all modified records from the database cache to disk.

There is one flag that you can set to customize <a href="../../api/c/dbclose.md" class="olink">DB-&gt;close()</a>:

<span class="term"> <a href="../../api/c/dbclose.md#dbclose_DB_NOSYNC" class="olink">DB_NOSYNC</a> </span>  
Do not flush cached information to disk.

**It is important to understand that flushing cached information to disk only minimizes the window of opportunity for corrupted data, it does not eliminate the possibility.**

While unlikely, it is possible for database corruption to happen if a system or application crash occurs while writing data to the database. To ensure that database corruption never occurs, applications must either:

- Use transactions and logging with automatic recovery.
- Use logging and application-specific recovery.
- Edit a copy of the database, and, once all applications using the database have successfully called <a href="../../api/c/dbclose.md" class="olink">DB-&gt;close()</a>, use system operations (for example, the POSIX rename system call) to atomically replace the original database with the updated copy.
