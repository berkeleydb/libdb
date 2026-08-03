---
title: "Berkeley DB 4.5.20 Change Log"
api-name: "Berkeley DB 4.5.20 Change Log"
source: docs/upgrading/changelog_4_5_20.html
---
## Berkeley DB 4.5.20 Change Log

<span class="sect2"> [Database or Log File On-Disk Format Changes:](changelog_4_5_20.md#idp50532016) </span>

<span class="sect2"> [New Features:](changelog_4_5_20.md#idp50511048) </span>

<span class="sect2"> [Database Environment Changes:](changelog_4_5_20.md#idp50513672) </span>

<span class="sect2"> [Concurrent Data Store Changes:](changelog_4_5_20.md#idp50516704) </span>

<span class="sect2"> [General Access Method Changes:](changelog_4_5_20.md#idp50520456) </span>

<span class="sect2"> [Btree Access Method Changes:](changelog_4_5_20.md#idp50542544) </span>

<span class="sect2"> [Hash Access Method Changes:](changelog_4_5_20.md#idp50536608) </span>

<span class="sect2"> [Queue Access Method Changes:](changelog_4_5_20.md#idp50533200) </span>

<span class="sect2"> [Recno Access Method Changes:](changelog_4_5_20.md#idp50539504) </span>

<span class="sect2"> [C++-specific API Changes:](changelog_4_5_20.md#idp50539760) </span>

<span class="sect2"> [Java-specific API Changes:](changelog_4_5_20.md#idp50541672) </span>

<span class="sect2"> [Java collections and bind API Changes:](changelog_4_5_20.md#idp50542632) </span>

<span class="sect2"> [Tcl-specific API Changes:](changelog_4_5_20.md#idp50546176) </span>

<span class="sect2"> [RPC-specific Client/Server Changes:](changelog_4_5_20.md#idp50548752) </span>

<span class="sect2"> [Replication Changes:](changelog_4_5_20.md#idp50547824) </span>

<span class="sect2"> [XA Resource Manager Changes:](changelog_4_5_20.md#idp50557816) </span>

<span class="sect2"> [Locking Subsystem Changes:](changelog_4_5_20.md#idp50534496) </span>

<span class="sect2"> [Logging Subsystem Changes:](changelog_4_5_20.md#idp50532216) </span>

<span class="sect2"> [Memory Pool Subsystem Changes:](changelog_4_5_20.md#idp50542056) </span>

<span class="sect2"> [Transaction Subsystem Changes:](changelog_4_5_20.md#idp50543016) </span>

<span class="sect2"> [Utility Changes:](changelog_4_5_20.md#idp50556608) </span>

<span class="sect2"> [Configuration, Documentation, Portability and Build Changes:](changelog_4_5_20.md#idp50557880) </span>

### Database or Log File On-Disk Format Changes:

1.  The on-disk log format has changed.

### New Features:

1.  Multi-Version Concurrency Control for the Btree/Recno access methods.
2.  A new replication framework with a default TCP/IP setup.
3.  Online replication upgrades for high availability replicated 24/7 systems.
4.  A new event-style notification.
5.  Several enhancements to the Java Collections API including the implementation of the size() method.

### Database Environment Changes:

1.  Update the DB_ENV-\>failchk method to garbage collect per-process mutexes stranded after unexpected process failure. \[#13964\]
2.  Fix a bug that could cause memory used to track threads for DB_ENV-\>failchk to not be reused when a thread no longer exists. \[#14425\]
3.  Add set_event_notify behavior as part of new event notification in Berkeley DB. \[#14534\]
4.  Fix a bug so that we no longer panic on DB_ENV-\>close() if a previous environment close failed to log. This condition will now return an error. \[#14693\]
5.  Created os_getenv, removed clib/getenv, implemented Windows specific behavior. \[#14942\]
6.  Fix a bug where it was possible to corrupt the DB_REGISTER information file, making it impossible for Berkeley DB applications to join database environments. \[#14998\]

### Concurrent Data Store Changes:

1.  Fix a bug where renaming a subdatabase in a Concurrent Data Store environment could fail. \[#14185\]

### General Access Method Changes:

1.  Fix a bug that could leave extra unallocated pages at the end of a database file. \[#14031\]
2.  Optimize secondary updates when overwriting primary records. \[#14075\]
3.  Fix a bug to prevent a trap when creating a named in-memory database and there are already temporary files open. \[#14133\]
4.  Fix a bug which caused a trap if the key parameter to DBC-\>c_get was omitted with DB_CURRENT. \[#14143\]
5.  Fix a bug with secondary cursors when the secondary has off-page duplicates. This bug resulted in incorrect primary data being returned. \[#14240\]
6.  Improve performance when removing a subdatabase by not locking every page. \[#14366\]
7.  Fix a bug that would not properly upgrade database files from releases 3.2.9 (and earlier) to releases 4.0 (and greater). \[#14461\]
8.  Fix a bug that could cause a DB_READ_UNCOMMITTED get through a secondary index to return DB_SECONDARY_CORRUPT. \[#14487\]
9.  Fix a bug so that non-transactional cursor updates of a transactional database will generate an error. \[#14519\]
10. Add a message when the system panics due to a page in the wrong state at its time of allocation. \[#14527\]
11. Fix a remove failure when attempting to remove a file that is open in another thread of control. \[#14780\]
12. Fix a bug where the key was not ignored when doing a cursor put with the DB_CURRENT flag. \[#14988\]

### Btree Access Method Changes:

1.  When deleting a page don't check the next key in the parent if we are going to delete the parent too.
2.  Need to check that the tree has not collapsed between dropping a read lock and getting the write lock. If it has collapsed we will fetch the root of the tree.
3.  Fix a case where we fail to lock the next page before reading it.

<!-- -->

1.  Changed the implementation of internal nodes in btrees so that they no longer share references to overflow pages with leaf nodes. \[#10717\]
2.  Fix a bug that could cause a diagnostic assertion by setting the deleted bit on a record in an internal node. \[#13944\]
3.  Fix three problems in BTREE compaction: \[#14238\]
4.  Fix a bug that could cause the compaction of a Btree with sorted duplicates to fail when attempting to compact an off page duplicate tree if a key could not fit in an internal node. \[#14771\]
5.  Fix a bug that causes a loop if an empty Btree was compacted. \[#14493\]

### Hash Access Method Changes:

1.  Fix a bug to allow creation of hash pages during truncate recovery. \[#14247\]

### Queue Access Method Changes:

1.  Fix a bug where reads of data items outside the range of the queue were not kept locked to the end of the transaction, breaking serializability. \[#13719\]
2.  Fix a bug that could cause corruption in queue extent files if multiple processes tried to open the same extent at the same time. \[#14438\]
3.  Improve concurrency for in-place updates in queue databases. \[#14918\]

### Recno Access Method Changes:

None.

### C++-specific API Changes:

1.  C++ applications that check the error code in exceptions should note that DbMemoryException has been changed to have the error code DB_BUFFER_SMALL rather than ENOMEM, to match the error returned by the C API. DbMemoryException will be thrown when a Dbt is too small to contain data returned by Berkeley DB. When a call to malloc fails, or some other resource is exhausted, a plain DbException will be thrown with error code set to ENOMEM. \[#13939\]

### Java-specific API Changes:

1.  Database.verify may now be called. This method is now static and takes a DatabaseConfig parameter. \[#13971\]
2.  Add DB_ENV-\>{fileid_reset, lsn_reset} to the public API. \[#14076\]

### Java collections and bind API Changes:

1.  The com.sleepycat.collections package is now fully compatible with the Java Collections framework. \[#14732\]

### Tcl-specific API Changes:

1.  Fix a conflicting variable, sysscript.tcl. \[#15051\]

### RPC-specific Client/Server Changes:

None.

### Replication Changes:

1.  Fix a bug when running with DEBUG_ROP or DEBUG_WOP. \[#13394\]
2.  Add live replication upgrade support \[#13670\]
3.  Fix a bug so that client databases are removed at the start of internal initialization. \[#14147\]
4.  Fix a bug in replication internal initialization so that data_dir will be handled correctly. Make internal initialization resilient to multiple data_dir calls with the same directory. \[#14489\]
5.  Fix a bug in the 4.2 sync-up algorithm that could result in no open files. \[#14552\]
6.  Fix a bug when clients decide to re-request. \[#14642\]
7.  Fix a bug where a PERM bulk buffer could have a zero LSN passed to the application callback. \[#14675\]
8.  Change names of some existing replication API methods as described in <a href="upgrade_4_5_rep_set.md" class="xref" title="Replication method naming">Replication method naming</a>. \[#14723\]
9.  Fix a bug which could cause an election to succeed only after waiting for the timeout to expire, even when all sites responded in a timely manner. The bug was most easily visible in an election between 2 sites. \[#14752\]
10. Fix a bug where a process could have an old file handle to a log file. \[#14797\]
11. Fix a bug where a "log_more" message could be on a log file boundary. \[#15034\]
12. Fix a bug that could cause log corruption if a database open operation were attempted during a call to rep_start in another thread. \[#15035\]
13. Fix a bug during elections where a vote2 arrives before its vote1. \[#15055\]
14. Fix a bug to make sure we are a client if sending a REP_REREQUEST. \[#15066\]

### XA Resource Manager Changes:

None.

### Locking Subsystem Changes:

1.  Fix a bug that could cause a write to hang if DB_READ_UNCOMMITTED is enabled and it tries to reacquire a write lock. \[#14919\]

### Logging Subsystem Changes:

1.  Fix a bug so that log headers are now included in the checksum. This avoids a possible race in doing hot backups. \[#11636\].
2.  Add a check so that some log sequence errors are diagnosed at run time rather than during recovery. \[#13231\]
3.  Fix a bug where recovery fails if there is no disk space for the forced checkpoint that occurs at the end of processing the log. \[#13986\]
4.  Fix a bug which could cause a page to be missing from the end of a database file if the page at the end of the file was freed while it contained data and the system was restarted before the log record for that free was flushed to disk. \[#14090\]
5.  Fix a bug that could cause log files to be incorrectly removed by log_archive if it was run immediately after recovery. \[#14874\]

### Memory Pool Subsystem Changes:

1.  Fix a bug that could cause corruption to the buffer pool cache if a race condition was hit while using DB-\>compact. \[#14360\]
2.  Fix a bug where cache pages could be leaked in applications creating temporary files for which the DB_MPOOL_NOFILE flag was set. \[#14544\]

### Transaction Subsystem Changes:

1.  Fix a bug that could cause extra empty pages to appear in a database file after recovery. \[#11118\]
2.  Fix a bug triggered when running recovery with a feedback function that could cause a NULL pointer dereference. \[#13834\]
3.  Fix a bug where running recovery could create duplicate entries in the data directory list. \[#13884\]
4.  Fix a bug to not trade locks if a write lock is already owned. \[#13917\]
5.  Fix a bug that could cause traps or hangs if the DB_TXN-\>set_name function is used in a multithreaded application. \[#14033\]
6.  Fix a bug so that a transaction can no longer be committed after it had deadlocked. \[#14037\]
7.  Fix a bug that could cause a trap during recovery if multiple operations that could remove the same extent are recovered. \[#14061\]
8.  Fix a bug that could cause an extent file to be deleted after the last record in the extent was consumed but the consuming transaction was aborted. \[#14179\]
9.  Fix a bug where the parent database would not use DB_READ_UNCOMMITTED in certain cases when calling DBC-\>c_pget. \[#14361\]
10. Fix a bug so that it is no longer possible to do a non-transactional cursor update on a database that is opened transactionally. \[#14519\]
11. Fix a bug that causes a sequence to ignore the DB_AUTO_COMMIT settings. \[#14582\]
12. Fix a bug, change txn_recover so that multiple processes will recover prepared transactions without requiring that the first process stay active. \[#14707\]
13. Fix a bug that could cause the wrong record to be deleted if a transaction had a cursor on a record with a pending delete and then replaced a record that contained overflow data or replaced a record with overflow data and that replace failed. \[#14834\]

### Utility Changes:

1.  Fix a bug that caused db_verify to not check the order on leaf pages which were the leftmost children of an internal node. \[#13004\]
2.  Fix a bug that caused db_hotbackup to not backup queue extent files. \[#13848\]
3.  Fix a bug so that db_verify no longer reports that an unused hash page is not fully zeroed. \[#14030\]
4.  Fix a bug where db_stat ignored the -f option to return "fast statistics". \[#14283\]
5.  Fix a bug that prevented the db_stat utility from opening database files with write permission so that meta data statistics would be updated. \[#14755\]
6.  Fix a bug in db_hotbackup related to windows. Sub-directories are now ignored. \[#14757\]

### Configuration, Documentation, Portability and Build Changes:

1.  The Berkeley DB 4.3 and 4.4 releases disallowed using the --with-uniquename configuration option with the C++, Java, or RPC --enable-XXX options. The 4.5 release returns to the 4.2 release behavior, allowing those combinations of configuration options. \[#14067\]
2.  Fix build issues when CONFIG_TEST is not enabled for Tcl. \[#14507\]
3.  There are updated build instructions for Berkeley DB PHP module on Linux. \[#14249\]
4.  Use libtool's "standard" environment variable names so that you can set "AR" to "ar -X64" for example, and modify both libtool and the Makefile commands. Remove the install-strip target from the Makefile, it is no longer used. \[#14726\]
5.  Fix a bug where, when a database is opened with the DB_THREAD flag (the default in Java), and an operation in one thread causes the database to be truncated (typically when the last page in the database is freed) concurrently with a read or write in another thread, there can be arbitrary data loss, as Windows zeros out pages from the read/write location to the end of the file. \[#15063\]
