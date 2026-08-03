---
title: "Berkeley DB 4.4.16 Change Log"
api-name: "Berkeley DB 4.4.16 Change Log"
source: docs/upgrading/changelog_4_4_16.html
---
## Berkeley DB 4.4.16 Change Log

<span class="sect2"> [Database or Log File On-Disk Format Changes:](changelog_4_4_16.md#idp50595920) </span>

<span class="sect2"> [New Features:](changelog_4_4_16.md#idp50583264) </span>

<span class="sect2"> [Database Environment Changes:](changelog_4_4_16.md#idp50583648) </span>

<span class="sect2"> [Concurrent Data Store Changes:](changelog_4_4_16.md#idp50567656) </span>

<span class="sect2"> [General Access Method Changes:](changelog_4_4_16.md#idp50591960) </span>

<span class="sect2"> [Btree Access Method Changes:](changelog_4_4_16.md#idp50592384) </span>

<span class="sect2"> [Hash Access Method Changes:](changelog_4_4_16.md#idp50595984) </span>

<span class="sect2"> [Queue Access Method Changes:](changelog_4_4_16.md#idp50597856) </span>

<span class="sect2"> [Recno Access Method Changes](changelog_4_4_16.md#idp50598936) </span>

<span class="sect2"> [C++-specific API Changes:](changelog_4_4_16.md#idp50598072) </span>

<span class="sect2"> [Java-specific API Changes:](changelog_4_4_16.md#idp50600424) </span>

<span class="sect2"> [Java collections and bind API Changes:](changelog_4_4_16.md#idp50621112) </span>

<span class="sect2"> [Tcl-specific API Changes:](changelog_4_4_16.md#idp50604672) </span>

<span class="sect2"> [RPC-specific Client/Server Changes:](changelog_4_4_16.md#idp50589536) </span>

<span class="sect2"> [Replication Changes:](changelog_4_4_16.md#idp50610200) </span>

<span class="sect2"> [XA Resource Manager Changes:](changelog_4_4_16.md#idp50594920) </span>

<span class="sect2"> [Locking Subsystem Changes:](changelog_4_4_16.md#idp50614600) </span>

<span class="sect2"> [Logging Subsystem Changes:](changelog_4_4_16.md#idp50614888) </span>

<span class="sect2"> [Memory Pool Subsystem Changes:](changelog_4_4_16.md#idp50635800) </span>

<span class="sect2"> [Transaction Subsystem Changes:](changelog_4_4_16.md#idp50617400) </span>

<span class="sect2"> [Utility Changes:](changelog_4_4_16.md#idp50617824) </span>

<span class="sect2"> [Configuration, Documentation, Portability and Build Changes:](changelog_4_4_16.md#idp50621200) </span>

### Database or Log File On-Disk Format Changes:

1.  The on-disk log format has changed.

### New Features:

1.  Add support to compact an existing Btree database. \[#6750\]
2.  Add support for named in-memory databases. \[#9927\]
3.  Add support for database environment recovery serialization. This simplifies multiprocess application architectures. Add DB_REGISTER flag to DB_ENV-\>open(). \[#11511\]
4.  Add utility for performing hot backups of a database environment. \[#11536\]
5.  Add replication configuration API. \[#12110\]
6.  Add replication support to return error instead of waiting for client sync to complete. \[#12110\]
7.  Add replication support for delayed client synchronization. \[#12110\]
8.  Add replication support for client-to-client synchronization. \[#12110\]
9.  Add replication support for bulk transfer. \[#12110\]
10. Add new flags DB_DSYNC_DB and DB_DSYNC_LOG \[12941\]
11. Add DbEnv.log_printf, a new DbEnv method which logs printf style formatted strings into the Berkeley DB database environment log. \[#13241\]

### Database Environment Changes:

1.  Add a feature to support arbitrary alignment of mutexes in order to minimize cache line collisions. \[#9580\]
2.  Change cache regions on 64-bit machines to allow regions larger than 4GB. \[#10668\]
3.  Fix a bug where a loop could occur if the application or system failed during modification of the linked list of shared regions. \[#11532\]
4.  Fix mutex alignment on Linux/PA-RISC, add test-and-set mutexes for MIPS and x86_64. \[#11575\]
5.  Fix a bug where private database environments (DB_PRIVATE) on 64-bit machines would core dump because of 64-bit address truncation. \[#11983\]
6.  Fix a bug where freed memory is accessed when DB_PRIVATE environments are closed. This can happen on systems where the operating system holds mutex resources that must be freed when the mutex is destroyed. \[#12591\]
7.  Fix a bug where the DbEnv.stat_print method could self-deadlock and hang. The DbEnv.stat_print method no longer displays statistics for any of the database environments databases. \[#12039\]
8.  Fix a bug where Berkeley DB could create fragmented filesystem-backed shared region files. \[#12125\]
9.  Fix a bug where Berkeley DB stat calls could report a cache size of 0 after the statistics were cleared. \[#12307\]
10. Threads of control joining database environments are now configured for all of the subsystems (lock, log, cache, or transaction) for which the environment was originally configured, it is now an error to attempt configuration of additional subsystems after an environment is created. \[#12422\]
11. Fix a bug where negative percentages could be displayed in statistics output. \[#12673\]
12. Fix a bug that could cause a panic if the cache is filled with non-logging updated pages. \[#12763\]
13. Fix a bug that could cause an unreported deadlock if the application was using the DB_DIRTY_READ flag and the record was an off page duplicate record. \[#12893\]
14. Fix a bug where a handle lock could be incorrectly retained during a delete or rename operation. \[#12906\]

### Concurrent Data Store Changes:

1.  Lock upgrades and downgrades are now accounted for separately from lock requests and releases. \[#11155\]
2.  Fix a bug where a second process joining a Concurrent Data Store environment, with the DB_CDB_ALLDB flag set, would fail. This would happen if the first thread were not entirely finished with initialization. \[#12277\]

### General Access Method Changes:

1.  Fix a bug where filesystem operations are improperly synchronized. \[#10564\]
2.  Add support for database files larger than 2GB on Windows. \[#11839\]
3.  Rename DB_DEGREE_2 (and all related flags) to DB_READ_COMMITTED. Rename DB_DIRTY (and all related flags) to DB_READ_UNCOMMITTED. \[#11776\]
4.  Fix a bug where wrapping of sequences was incorrect when the cache size is smaller than the range of the maximum value minus the minimum value. \[#11997\]
5.  Fix a bug that could result in a hot backup having a page missing from a database file if a file truncation was in progress during the backup but was then aborted. \[#12017\]
6.  Fix a bug where a long filename could cause one too few bytes to be allocated when opening a file. \[#12085\]
7.  Fix a bug in secondary cursor code if a write lock is not granted. \[#12109\]
8.  Fix a bug in secondary cursors where the current record would change on error. \[#12141\]
9.  Fix a bug in Db-\>truncate where the method was not checking to see if the handle was opened read-only. \[#12179\]
10. Fix a bug in sequences so that they are now platform independent, taking into account little-endian and big-endian architectures. They will be automatically upgraded in 4.4. \[#12202\]
11. Fix a bug with non-wrapping sequences when initial value was INT64_MIN. \[#12390\]
12. Add a retry for operating system operations that return EIO (IO Error) to better support NFS mounted filesystems. \[#12426\]
13. Fix sequence wrapping at INT64 limits. \[#12520\]
14. Fix a bug where errors during DB-\>associate could leave secondaries half associated. \[#13173\]
15. Fix a bug so that we no longer will update in CDS and DS if the file size limit will be exceeded. \[#13222\]

### Btree Access Method Changes:

1.  Remove maxkey configuration. \[#8904\]
2.  Fix a memory leak in operations on large Btrees. \[#12000\]

### Hash Access Method Changes:

1.  Fix a bug where access to HASH or encrypted database pages might be blocked during a checkpoint. \[#11031\]
2.  Fix a bug where recovery would fail when a database has a hash page on the free list and that hash page was freed without using transactions and later allocated and aborted within a transaction. \[#11214\]
3.  Fix a bug in hash duplicates where if the caller left garbage in the partial length field, we were using it. Fix a bug where a replacement of a hash item that should have gone on an overflow page, did not. \[#11966\]
4.  Fix a bug where free space was miscalculated when adding the first duplicate to an existing item and the existing item plus the new item does not fit on a page. \[#12270\]
5.  Fix a bug where allocations of hash buckets are not recovered correctly. \[#12846\]

### Queue Access Method Changes:

1.  Improve performance of deletes from a QUEUE database that does not have a secondary index. \[#11538\]
2.  Fix a bug where updates that do not use transactions, but do enable locking, failed to release locks. \[#11669\]
3.  Fix a bug where a transaction might not be rolled forward if the site was performing hot backups and an application aborted a prepared but not committed transaction. \[#12181\]
4.  Fix a bug with queue extents not being reclaimed. \[#12249\]
5.  Fix a bug where a record being inserted before the head of the queue could appear missing if DB_CONSUME is not specified. \[#12919\]
6.  Fix a bug that might cause recovery to move the head or tail of the queue to exclude a record that was deleted but whose transaction did not commit. \[#13256\]
7.  Fix a bug that could cause recovery to move the head or tail pointer beyond a record that was aborted but was rolled backward by recovery. \[#13318\]

### Recno Access Method Changes

None.

### C++-specific API Changes:

1.  Fix a bug so that a DbMemoryException will be raised during a DB_BUFFER_SMALL error. \[#13273\]

### Java-specific API Changes:

1.  Add VersionMismatchException to map the DB_VERSION_MISMATCH error. \[#11429\]
2.  Fix a bug in Environment.getConfiguration() method in non-crypto builds. \[#11752\]
3.  Fix a bug that caused a NullPointerException when using the MultipleDataEntry default constructor. \[#11753\]
4.  Fix handling of replication errors. \[#11822\]
5.  Remove EnvironmentConfig.setReadOnly() method. \[#11882\]
6.  Fix a bug where prefix strings in the error handler may be corrupted. \[#11967\]
7.  Fix a bug so that nested exceptions will appear in stack traces. \[#11992\]
8.  Fix a bug on LogSequenceNumber objects in the Java API. \[#12223\]
9.  Fix a bug when no files are returned from a call to DB_ENV-\>log_archive. \[#12383\]
10. Fix a bug when multiple verbose flags are set. \[#12383\]
11. Fix a bug so that an OutOfMemoryError is thrown when allocation fails in the JNI layer. \[#13434\]

### Java collections and bind API Changes:

1.  Binding performance has been improved by using System.arraycopy in the FastOutputStream and FastInputStream utility classes. \[#12002\]
2.  The objectToEntry method is now implemented in all TupleBinding subclasses (IntegerBinding, etc) so that tuple bindings are fully nestable. An example of this usage is a custom binding that dynamically discovers the data types of each of the properties of a Java bean class. For each property, it calls TupleBinding.getPrimitiveBinding using the property's type (class). When the custom binding's objectToEntry method is called, it in turn calls the objectToEntry method of the nested bindings for each property. \[#12124\]
3.  The getCause method for IOExceptionWrapper and RuntimeExceptionWrapper is now defined so that nested exceptions appear in stack traces for exceptions thrown by the collections API. \[#11992\]
4.  TupleBinding.getPrimitiveBinding can now be passed a primitive type class as well as a primitive wrapper class. The return value for Integer.TYPE and Integer.class, for example, will be the same binding. \[#12035\]
5.  Improvements have been made to prevent the buffer used in serial and tuple bindings from growing inefficiently, and to provide more alternatives for the application to specify the desired size. For details see com.sleepycat.bind.serial.SerialBase and com.sleepycat.bind.tuple.TupleBase. \[#12398\]
6.  Add StoredContainer.getCursorConfig, deprecate isDirtyRead. Deprecate StoredCollections.dirtyReadMap (dirtyReadSet, etc) which is replaced by configuredMap (configuredSet, etc). Deprecated StoredContainer.isDirtyReadAllowed with no replacement (please use DatabaseConfig.getDirtyRead). Also note that StoredCollections.configuredMap (configuredSet, etc) can be used to configure read committed and write lock containers, as well as read uncommitted containers, since all CursorConfig properties are supported. \[#11776\]
7.  Add the protected method SerialBinding.getClassLoader so that subclasses may return a specific or dynamically determined class loader. Useful for applications which use multiple class loaders, including applications that serialize Groovy-defined classes. \[#12764\] \[#12749\]

### Tcl-specific API Changes:

1.  Fix a bug that could cause a memory leak in the replication test code. \[#13436\]

### RPC-specific Client/Server Changes:

1.  Fix double-free in RPC server when handling an out-of-memory error. \[#11852\]

### Replication Changes:

1.  Fix race condition (introduced in 4.3) in rep_start function. \[#11030\]
2.  Changed internal initialization to no longer store records. \[#11090\]
3.  Add support for replication bulk transfer. \[#11099\]
4.  Berkeley DB now calls check_doreq function for MASTER_REQ messages. \[#11207\]
5.  Fix a bug where transactions could be counted incorrectly during txn_recover. \[#11257\]
6.  Add DB_REP_IGNORE flag so that old messages (especially PERM messages) can be ignored by applications. \[#11585\]
7.  Fix a bug where op_timestamp was not initialized. \[#11795\]
8.  Fix a bug in db_refresh where a client would write a log record on closing a file. \[#11892\]
9.  Fix backward arguments in C++ rep_elect API. \[#11906\]
10. Fix a bug where a race condition could happen between downgrading a master and a database update operation. \[#11955\]
11. Fix a bug on VERIFY_REQ. We now honor wait recs/rcvd. \[#12097\]
12. Fix a bug in rebroadcast of verify_req by initializing lp-\>wait_recs when finding a new master. \[#12097\]
13. Fix a bug by adding lockout checking to \_\_env_rep_enter since rename/remove now call it. \[#12192\]
14. Fix a bug so that we now skip \_\_db_chk_meta if we are a rep client. \[#12316\]
15. Fix a replication failure on Windows. \[#12331\]
16. Remove master discovery phase from rep_elect as a performance improvement to speed up elections. \[#12551\]
17. Fix a bug to avoid multiple data streams when issuing al ALL_REQ. \[#12595\]
18. Fix a bug to request the remaining gap again if the gap record is dropped after we receive the singleton. \[#12974\]
19. Fix a bug in internal initialization when master changes in the middle of initializing. \[#13074\]
20. Fix a bug in replication/archiving with internal init. \[#13110\]
21. Fix pp handling of db_truncate. \[#13115\]
22. Fix a bug where rep_timestamp could be updated when it should not be updated. \[#13331\]
23. Fix a bug with bulk transfer when toggling during updates. \[#13339\]
24. Change EINVAL error return to DB_REP_JOIN_FAILURE. \[#12110\]
25. Add C++ exception for DB_REP_HANDLE_DEAD. \[#13361\]
26. Fix a bug where starting an election concurrently with processing a NEWMASTER message could cause the send function to be called with an invalid eid. \[#13403\]

### XA Resource Manager Changes:

None.

### Locking Subsystem Changes:

None.

### Logging Subsystem Changes:

1.  Add set_log_filemode for applications that need to set an absolute file mode on log files. \[#8747\]
2.  Fix a bug that caused Not Found to be returned if a log file exists but is not readable. \[#11185\]
3.  Removed checksum of records with an in-memory log buffer. \[#11280\]
4.  Fix a bug so that the DB_LOG_INMEMORY flag can no longer be set after calling DB_ENV-\>open. \[#11436\]
5.  Fix a bug introduced after release 4.0 where two simultaneous checkpoints could cause ckp_lsn values to be out of order. \[#12094\]
6.  Fix a bug when in debug mode and using the DEBUG_ROP which will now log read operations in \_\_dbc_logging. \[#12303\]
7.  Fix a bug where failing to write a log record on a file close would result in a core dump later. \[#12460\]
8.  Fix a bug where automatic log file removal, or the return of log files using an absolute path, could fail silently if the applications current working directory could not be reached using the systems getcwd library call. \[#12505\]
9.  Avoid locking the log region if we are not going to flush the log. This can improve performance for some write-intensive application workloads. \[#13090\]
10. Fix a bug with a possible segment fault when memp_stat_print is called on a temporary database. \[#13315\]
11. Fix a bug where log_stat_print could deadlock with threads during a checkpoint. \[#13315\]

### Memory Pool Subsystem Changes:

1.  Fix a bug where modified database pages might not be flushed if recovery were run and all pages from a database were found in the system cache and up to date, followed by a system crash. \[#11654\]

### Transaction Subsystem Changes:

1.  Add new DbTxn class methods allowing applications to set/get a descriptive name associated with a transaction. The descriptive name is also displayed by the db_stat utility. \[#0382\]
2.  Fix a bug where aborting a transaction with a large number of nested transactions could take a long time. \[#10972\]
3.  Add support to allow the TXN_WRITE_NOSYNC flag to be specified on the transaction handle. \[#11151\]
4.  Fix a bug that could cause a page to be on the free list twice if it was originally put on the free list by a non-transactional update and then reallocated in a transaction that aborts. \[#11159\]
5.  Remove the requirement for the DB_AUTO_COMMIT flag to make database operations transactional. Specifying the database environment as transactional or opening the database handle transactionally is sufficient. \[#11302\]
6.  Fix a bug so that environments created from errant programs that called dbp-\>close while transactions were still active can now be recovered. \[#11384\]
7.  Fix a bug that caused free pages at the end of a file to be truncated during recovery rather than placed on the free page list. \[#11643\]
8.  Fix a bug that caused a page to have the wrong type if the truncate of a BREE or RECNO database needed to be rolled forward. \[#11670\]
9.  Fix a bug when manually undoing a subdb create, dont try to free a root page that has not been allocated. \[#11925\]
10. Add a check on database open to see if log files were incorrectly removed by system administration mistakes. \[#12178\]
11. Fix a bug when calling DB-\>pget and then specifying the DB_READ_COMMITTED (DB_DEGREE_2) on a cursor. If followed by a DBC-\>c_pget, the primary database would incorrectly remain locked. \[#12410\]
12. Fix a bug where the abort of a transaction in which a sub database was opened with the DB_TXN_NOT_DURABLE flag could fail. \[#12420\]
13. Fix a bug that could cause an abort transaction that allocated new pages to a file that were not flushed to disk prior to the abort transaction to report out of disk space. \[#12743\]
14. Fix a bug that could prevent multiple creates and destroys of the same file to be recovered correctly. \[#13026\]
15. Fix a bug when recovery previously handled a section of the log that did not contain any transactions. \[#13139\]
16. Fix a bug that could result in the loss of durability in Transactional Environments on Mac OS X. \[#13149\]
17. Fix a bug that could cause the improper reuse of a transaction id when recovery restores prepared transactions. \[#13256\]

### Utility Changes:

1.  Add utility for performing hot backups of a database environment. \[#11536\]
2.  Change the Verify utility to now identify any nodes that have incorrect record counts. \[#11934\]
3.  Fix a bug in the 1.85 compatibility code supporting per-application Btree comparison and prefix compression functions. The functions would not work on big-endian 64-bit hardware. \[#13316\]

### Configuration, Documentation, Portability and Build Changes:

1.  Change the ex_tpcb sample application to no longer displays intermediate results. It displays results at the end of the run. \[#11259\]
2.  Change the Visual Studio projects on Windows so that each is in an intermediate directory. \[#11441\]
3.  Fix errors in test subdb011. \[#11799\]
4.  Fix a bug that could cause applications using gcc on Power PC platforms to hang. \[#12233\]
5.  Fix a bug where installation will fail if a true program cannot be found. \[#12278\]
6.  Fix a bug that prevented C++ applications from configuring XA \[#12300\].
7.  Fix a race condition in the Windows mutex implementation found on 8-way Itanium systems. \[#12417\]
8.  Add pthread mutex support for IBM OS/390 platform (z/OS or MVS). \[#12639\]
9.  Fix a bug where the Tcl API did not configure on OS X 10.4. \[#12699\]
10. Fix portability issues with queue or recno primary databases. \[#12872\]
11. Fix a bug where utility attempted to send replication message. \[#13446\]
