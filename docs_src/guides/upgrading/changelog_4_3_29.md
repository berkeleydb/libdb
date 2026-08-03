---
title: "Berkeley DB 4.3.29 Change Log"
api-name: "Berkeley DB 4.3.29 Change Log"
source: docs/upgrading/changelog_4_3_29.html
---
## Berkeley DB 4.3.29 Change Log

<span class="sect2"> [Database or Log File On-Disk Format Changes:](changelog_4_3_29.md#idp50694880) </span>

<span class="sect2"> [New Features:](changelog_4_3_29.md#idp50670248) </span>

<span class="sect2"> [Database Environment Changes:](changelog_4_3_29.md#idp50673968) </span>

<span class="sect2"> [Concurrent Data Store Changes:](changelog_4_3_29.md#idp50703424) </span>

<span class="sect2"> [General Access Method Changes:](changelog_4_3_29.md#idp50690848) </span>

<span class="sect2"> [Btree Access Method Changes:](changelog_4_3_29.md#idp50691272) </span>

<span class="sect2"> [Hash Access Method Changes:](changelog_4_3_29.md#idp50694944) </span>

<span class="sect2"> [Queue Access Method Changes:](changelog_4_3_29.md#idp50697104) </span>

<span class="sect2"> [Recno Access Method Changes](changelog_4_3_29.md#idp50720352) </span>

<span class="sect2"> [C++-specific API Changes:](changelog_4_3_29.md#idp50700784) </span>

<span class="sect2"> [Java-specific API Changes:](changelog_4_3_29.md#idp50670632) </span>

<span class="sect2"> [Tcl-specific API Changes:](changelog_4_3_29.md#idp50702384) </span>

<span class="sect2"> [RPC-specific Client/Server Changes:](changelog_4_3_29.md#idp50703784) </span>

<span class="sect2"> [Replication Changes:](changelog_4_3_29.md#idp50685776) </span>

<span class="sect2"> [XA Resource Manager Changes:](changelog_4_3_29.md#idp50733112) </span>

<span class="sect2"> [Locking Subsystem Changes:](changelog_4_3_29.md#idp50712384) </span>

<span class="sect2"> [Logging Subsystem Changes:](changelog_4_3_29.md#idp50740760) </span>

<span class="sect2"> [Memory Pool Subsystem Changes:](changelog_4_3_29.md#idp50695328) </span>

<span class="sect2"> [Transaction Subsystem Changes:](changelog_4_3_29.md#idp50720440) </span>

<span class="sect2"> [Utility Changes:](changelog_4_3_29.md#idp50724480) </span>

<span class="sect2"> [Configuration, Documentation, Portability and Build Changes:](changelog_4_3_29.md#idp50724864) </span>

### Database or Log File On-Disk Format Changes:

1.  The on-disk log format has changed.

### New Features:

1.  Add support for light weight, transactionally protected Sequence Number generation. \[#5739\]
2.  Add support for Degree 2 isolation. \[#8689\]
3.  Add election generation information to replication to support Paxos compliance. \[#9068\]
4.  Add support for 64-bit and ANSI C implementations of the RPCGEN utility. \[#9548\]

### Database Environment Changes:

1.  Fix a bug where the permissions on system shared memory segments did not match the mode specified in the DB_ENV-\>open() method. \[#8921\]
2.  Add a new return error from the DB_ENV-\>open() method call, DB_VERSION_MISMATCH, which is returned in the case of an application compiled under one version of Berkeley DB attempting to open an environment created under a different version. \[#9077\]
3.  Add support for importing databases from a transactional database environment into a different environment. \[#9324\]
4.  Fix a bug where a core dump could occur if a zero-length database environment name was specified. \[#9233\]
5.  Increase the number of environment regions to 100. \[#9297\]
6.  Remove the DB_ENV-\>set_verbose() method flag DB_VERB_CHKPOINT. \[#9405\]
7.  Fix bugs where database environment getters could return incorrect information after the database environment was opened, if a different thread of control changed the database environment values. Fix bugs where database environment getter/setter functions could race with other threads of control. \[#9724\]
8.  Change the DbEnv.set_lk_detect method to match the DbEnv.open semantics. That is, DbEnv.set_lk_detect may be called after the database environment is opened, allowing applications to configure automatic deadlock detection if it has not yet been configured. \[#9724\]
9.  Fix cursor locks for environments opened without DB_THREAD so that they use the same locker ID. This eliminates many common cases of application self-deadlock, particularly in CDS. \[#9742\]
10. Fix a bug in DB-\>get_env() in the C API where it could return an error when it should only return the DB_ENV handle. C++ and Java are unchanged. \[#9828\]
11. Fix a bug where we only need to initialize the cryptographic memory region when MPOOL, Log or Transactions have been configured. \[#9872\]
12. Change private database environments at process startup to only allocate the heap memory required at any particular time, rather than always allocating the maximum amount of heap memory configured for the environment. \[#9889\]
13. Add a method to create nonexistent intermediate directories when opening database files. \[#9898\]
14. Add support for in-memory logging within database environments. \[#9927\]
15. Change Berkeley DB so configuring a database environment for automatic log file removal affects all threads in the environment, not just the DbEnv handle in which the configuration call is made. \[#9947\]
16. Change the signature of the error callback passed to the DB_ENV-\>set_errcall and DB-\>set_errcall methods to add a DB_ENV handle, to provide database environment context for the callback function. \[#10025\]
17. Fix a race condition between DB-\>close and DB-\>{remove,rename} on filesystems that don't allow file operations on open files (such as Windows). \[#10180\]
18. Add a DB_DSYNC_LOG flag to the DbEnv::set_flags method, which configures O_DSYNC on POSIX systems and FILE_FLAG_WRITE_THROUGH on Win32 systems. This offers significantly better performance for some applications on certain Solaris/filesystem combinations. \[#10205\]
19. Fix a bug where calling the DB or DBEnv database remove or rename methods could cause a transaction checkpoint or cache flush to fail. \[#10286\]
20. Change file operations not to flush a file if it hasn't been written. \[#10537\]
21. Remove 4GB restriction on region sizes on 64 bit machines. \[#10668\]
22. Simplify the signature of substitute system calls for ftruncate and seek. \[#10668\]
23. Change Berkeley DB so that opening an environment without specifying a home directory will cause the DB_CONFIG file in the current directory to be read, if it exists. \[#11424\]
24. Fix a bug that caused a core dump if DB handles without associated database environments were used for database verification. \[#11649\]
25. Fix Windows mutexes shared between processes run as different users. \[#11985\]
26. Fix Windows mutexes for some SMP machines. \[#12417\]

### Concurrent Data Store Changes:

1.  Fix cursor locks for environments opened without DB_THREAD so that they use the same locker ID. This eliminates many common cases of application self-deadlock, particularly in CDS. \[#9742\]

### General Access Method Changes:

1.  Fix a bug where Berkeley DB log cursors would close and reopen the underlying log file each time the log file was read. \[#8934\]
2.  Improve performance of DB-\>open() for existing subdatabases maintained within the same database file. \[#9156\]
3.  Add a new error, DB_BUFFER_SMALL, to differentiate from ENOMEM. The new error indicates that the supplied DBT is too small. ENOMEM is now always fatal. \[#9314\]
4.  Fix a bug when an update through a secondary index is deadlocked it is possible for the deadlock to be ignored, resulting in a partial update to the data. \[#9492\]
5.  Fix a bug where a record could get inserted into the wrong database when a page was deallocated from one subdatabase and reallocated to another subdatabase maintained within the same database file. \[#9510\]
6.  Enhance file allocation so that if the operating system supports decreasing the size of a file and the last page of the file is freed, it will be returned to the operating system. \[#9620\]
7.  Fix a bug where DB_RUNRECOVERY could be returned if there was no more disk space while aborting the allocation of a new page in a database. \[#9643\]
8.  Fix a bug where the cryptographic code could memcpy too many bytes. \[#9648\]
9.  Fix a bug with DB-\>join() cursors that resulted in a memory leak and incomplete results. \[#9763\]
10. Disallow cursor delete, followed a cursor put of the current item across all access methods. \[#9900\]
11. Fix a bug where recovery of operations on unnamed databases that were never closed, could fail. \[#10118\]
12. Fix a bug where DB-\>truncate of a database with overflow records that spanned more than one page would loop. \[#10151\]
13. Improve performance in the database open/close path. \[#10266\]
14. Fix a bug that restricted the number of temporary files that could be created to 127. \[#10415\]
15. Fix a bug which could cause a Too many files error when trying to create temporary files. Limit the number of temporary file creation retries. \[#10760\] \[#10773\]
16. Fix a memory leak bug with Sequence Numbers. \[#11589\]
17. Fix a bug on Windows platforms that prevents database files from growing to over 2GB. \[#11839\]
18. Fix a platform independence bug with sequence numbers. Existing sequence numbers will be automatically upgraded upon next access. \[#12202\]
19. Fix a race between truncate and read/write operations on Windows platforms that could cause corrupt database files. \[#12598\]

### Btree Access Method Changes:

1.  Fix a bug where a record could get placed on the wrong page when two threads are simultaneously trying to split a four level (or greater) Btree. \[#9542\]
2.  Fix a bug where calling DB-\>truncate() on a Btree which has duplicate keys that overflow the leaf page would not properly free the overflow pages and possibly loop. \[#10666\]

### Hash Access Method Changes:

1.  Fix a bug where a delete to a HASH database with off page duplicates could fail to have the proper lock when deleting an off page duplicate tree. \[#9585\]
2.  Fix a bug where a dirty reader using a HASH database would leave a lock on the meta page. \[#10105\]
3.  Fix a bug where a DB-\>del() on a HASH database supporting dirty reads could fail to upgrade a WWRITE lock to a WRITE lock when deleting an off page duplicate. \[#10649\]

### Queue Access Method Changes:

1.  Fix a bug where DB_CONSUME_WAIT may loop rather than wait for a new record to enter the queue, if the queue gets into a state where there are only deleted records between the head and the end of queue. \[#9215\]
2.  Fix a bug where a Queue extent file could be closed when it was empty, even if a thread was still accessing a page from that file. \[#9291\]
3.  Fix a bug where DBC-\>c_put(key, data, DB_CURRENT) where inserting a new record after the current record had been deleted was returning DB_KEYEMPTY. \[#9314\]
4.  Fix a bug where a Queue extent file could be reported as not found if a race condition was encountered between removing the file and writing out a stale buffer. \[#9462\]
5.  Fix a bug where the Queue access method might fail to release a record lock when running without transactions. \[#9487\]
6.  Add DB_INORDER flag for Queue databases to guarantee FIFO (First In, First Out) ordering when using DB_CONSUME or DB_CONSUME_WAIT. \[#9689\]
7.  Fix a bug where remove and rename calls could fail with a "Permission denied" error. \[#9775\]
8.  Fix a bug where aborting a transaction that opened and renamed a queue database using extents could leave some of the extent files with the wrong name on Windows. \[#9781\]
9.  Fix a bug where a db_dump of a queue database could return an error at the end of the queue if the head or tail of the queue is the first record on a page. \[#10215\]
10. Fix a race condition which would leave a Queue extent file open until the database handle was closed, preventing it from being removed. \[#10591\]
11. Fix a bug where a deadlock of a put on a database handle with dirty readers could generate a lock downgrade error. \[#10678\]
12. Fix a bug which caused DB_SET_RANGE and DB_GET_BOTH_RANGE to not return the next record when an exact match was not found. \[#10860\]

### Recno Access Method Changes

1.  Fix a bug where the key/data counts returned by the Db-\>stat method for Recno databases did not match the documentation. \[#8639\]
2.  Fix a bug where DBC-\>c_put(key, data, DB_CURRENT) where inserting a new record after the current record had been deleted was returning DB_KEYEMPTY. \[#9314\].

### C++-specific API Changes:

1.  Change DbException to extend std::exception, making it possible for applications to catch all exceptions in one place. \[#10022\]
2.  Fix a bug where errors during transaction destructors (commit, abort) could cause an invalid memory access. \[#10302\]
3.  Fix a bug that could lead to a read through an uninitialized pointer when a DbLockNotGrantedException is thrown. \[#10470\]
4.  Fix a bug in the C++ DbEnv::rep_elect method API where the arguments were swapped, leading to an "Invalid Argument" return when that method is called. \[#11906\]

### Java-specific API Changes:

1.  Fix a bug where the Java API did not respect non-zero return values from secondaryKeyCreate, including DB_DONOTINDEX. \[#9474\]
2.  Fix a bug where a self-deadlock occurred with a non-transactional class catalog database used in a transactional environment. The bug only occurred when the collections API was not used for starting transactions. \[#9521\]
3.  Improve Javadoc for the Java API. \[#9614\]
4.  Improve memory management and performance when large byte arrays are being passed to DB methods. \[#9801\]
5.  Improve performance of accessing statistics information from the Java API. \[#9835\]
6.  Allow Java application to run without DB_THREAD so they can be used as RPC clients. \[#10097\]
7.  Fix a bug where an uninitialized pointer is dereferenced for logArchive(Db.DB_ARCH_REMOVE). \[#10225\]
8.  Fix a bug in the Collections API where a deadlock exception could leave a cursor open. \[#10516\]
9.  Fix the replication callback in the Java API so that the parameter names match the C API. \[#10550\]
10. Add get methods to the Java statistics classes. \[#10807\]
11. Fix bugs in the Java API handling of null home directories and environments opened without a memory pool. \[#11424\]
12. Fix the Java API in the non-crypto package. \[#11752\]
13. Fix a bug that would cause corruption of error prefix strings. \[#11967\]
14. Fix handling of LSNs in the Java API. \[#12223\]
15. Dont throw a NullPointerException if the list of files returned by log_archive is empty. \[#12383\]

### Tcl-specific API Changes:

None.

### RPC-specific Client/Server Changes:

1.  Add support for 64-bit and ANSI C implementations of the RPCGEN utility. \[#9548\]
2.  Fix a small memory leak in RPC clients. \[#9595\]
3.  Fix a bug in the RPC server to avoid self-deadlock by always setting DB_TXN_NOWAIT. \[#10181\]
4.  Fix a bug in the RPC server so that if it times out an environment, it first closes all the Berkeley DB handles in that environment. \[#10623\]

### Replication Changes:

1.  Add an Environment ID to distinguish between clients from different replication groups. \[#7786\]
2.  Add number of votes required and flags parameters to DB_ENV-\>rep_elect() method. \[#7812\]
3.  Fix a bug where a client's env_openfiles pass could start with the wrong LSN. This could result in very long initial sync-up times for clients joining a replication group. \[#8635\]
4.  Add election generation information to replication to support Paxos compliance. \[#9068\]
5.  Add rep019 to test running normal recovery on clients to make sure we synch to the correct LSNs. \[#9151\]
6.  Remove support for logs-only replication clients. Use of the DB_REP_LOGSONLY flag to the DB_ENV-\>rep_start() method should be replaced with the DB_REP_CLIENT flag. \[#9331\]
7.  Fix a bug where replication clients fail to lock all the necessary pages when applying updates when there are more than one database in the transaction. \[#9569\]
8.  Fix a bug in replication elections where when elections are called by multiple threads the wrong master could get elected. \[#9770\]
9.  Fix a bug where the master could get a DB_REP_OUTDATED error. Instead send an OUTDATED message to the client. \[#9881\]
10. Add support for automatic initialization of replication clients. \[#9927\]
11. Modify replication timestamp so that non-replication client applications can get a DB_REP_HANDLE_DEAD. \[#9986\]
12. Add a new DB_REP_STARTUPDONE return value for rep_process_message() and st_startup_done to rep_stat() to indicate when a client has finished syncing up to a master and is processing live messages. \[#10310\]
13. Add \_pp to secondary handles, add RPRINT, fix a deadlock. \[#10429\]
14. Fix a bug where an old client (and no master) that dropped the ALIVE message would never update to the current generation. \[#10469\]
15. Fix a bug where a message could get sent to a new client before NEWSITE has been returned to the application. Broadcast instead. \[#10508\]
16. Fix a crash when verbose replication messages are configured and a NULL DB_LSN pointer is passed to rep_process_message. \[#10508\]
17. Add code to respect set_rep_limit in LOG_REQ processing. \[#10716\]
18. Fix a synchronization problem between replication recovery and database open. \[#10731\]
19. Change elections to adjust timeout if egen changes while we are waiting. \[#10686\]
20. Client perm messages now return ISPERM/NOTPERM instead of 0. \[#10855\] \[#10905\]
21. Fix a race condition during rep_start when a role change occurs. Fix memory leaks. \[#11030\]
22. Fix problems with duplicate records. A failure will no longer occur if the records are old records (LOG_MORE) and archived. \[#11090\]
23. Fix a bug where the replication temporary database would grow during automatic client initialization. \[#11090\]
24. Add throttling to PAGE_REQ. \[#11130\]
25. Remove optimization-causing problems with racing threads in rep_verify_match. \[#11208\]
26. Fix memory leaks. \[#11239\]
27. Fix an initialization bug when High Availability configurations are combined with private database environments, which can cause intermittent failures. \[#11795\]
28. Fix a bug in the C++ DbEnv::rep_elect method API where the arguments were swapped, leading to an "Invalid Argument" return when that method is called. \[#11906\]

### XA Resource Manager Changes:

None.

### Locking Subsystem Changes:

1.  Fix a bug where a deadlock of an upgrade from a dirty read to a write lock during an aborted transaction, may not be detected. \[#7143\]
2.  Add support for Degree 2 isolation. \[#8689\]
3.  Change the system to return DB_LOCK_DEADLOCK if a transaction attempts to get new locks after it has been selected as the deadlock victim. \[#9111\]
4.  Fix a bug where when configured to support dirty reads, a writer may not downgrade a write lock as soon as possible, potentially blocking dirty readers. \[#9197\]
5.  Change the test-and-set mutex implementation to avoid interlocked instructions when we know the instruction is unlikely to succeed. \[#9204\]
6.  Fix a bug where a thread supporting dirty readers can get blocked while trying to get a write lock. It will allocate a new lock rather than using an existing WAS_WRITE lock when it becomes unblocked, causing the application to hang. \[#10093\]
7.  The deadlock detector will now note that a parent transaction should be considered in abort if one of its children is. \[#10394\]
8.  Remove a deadlock where database closes could deadlock with page acquisition. \[#10726\]
9.  Fix a bug where a dirty reader could read an overflow page that was about to be deleted. \[#10979\]
10. Fix a bug that failed to downgrade existing write locks during a btree page split when supporting dirty reads. \[#10983\]
11. Fix a bug that would fail to upgrade a write lock when moving a cursor off a previously deleted record. \[#11042\]

### Logging Subsystem Changes:

1.  Fix a bug where recovery could leave too many files open. \[#9452\]
2.  Fix a bug where aborting a transaction with a file open in it could result in an unrecoverable log file. \[#9636\]
3.  Fix a bug where recovery would not return a fatal error if the transaction log was corrupted. \[#9841\]
4.  Fix a bug in recovery so that the final checkpoint no longer tries to flush the log. This will permit recovery to complete even if there is no disk space to grow the log file. \[#10204\]
5.  Improve performance of log flushes by pre-allocating log files and using fdatasync() in preference to fsync(). \[#10228\]
6.  Fix a bug where recovery of a page split after a non-transactional update to the next page would fail to update the back pointer. \[#10421\]
7.  Fix a bug in log_archive() where \_\_env_rep_enter() was called twice. \[#10577\]
8.  Fix a bug with in-memory logs that could cause a memory leak in the log region. \[#11505\]

### Memory Pool Subsystem Changes:

1.  Fix a bug in the MPOOLFILE file_written flag value so that checkpoint doesn't repeatedly open, flush and sync files in the cache for which there are no active application handles. \[#9529\]

### Transaction Subsystem Changes:

1.  Fix a bug where the same transaction ID could get allocated twice if you wrapped the transaction ID space twice and then had a very old transaction. \[#9036\]
2.  Fix a bug where a transaction abort that contained a page allocation could loop if the filesystem was full. \[#9461\]
3.  Fix implementation of DB-\>get_transactional() to match documentation: there is no possibility of error return, only 0 or 1. \[#9526\]
4.  Fix a bug where re-setting any of the DB_TXN_NOSYNC, DB_TXN_NOT_DURABLE and DB_TXN_WRITE_NOSYNC flags could fail to clear previous state, potentially leading to incorrect transactional behavior in the application. \[#9947\]
5.  Add a feature to configure the maximum number of files that a checkpoint will hold open. \[#10026\]
6.  An aborting transaction will no longer generate an undetected deadlock. \[#10394\]
7.  Fix a bug that prevented a child transaction from accessing a database handle that was opened by its parent transaction. \[#10783\]
8.  Fix a bug where a checkpoint or a delayed write in another process could raise an EINVAL error if the database had been opened with the DB_TXN_NOT_DURABLE flag. \[#10824\]
9.  Fix private transactional environments on 64-bit systems. \[#11983\]

### Utility Changes:

1.  Add debugging and performance tuning information to db_stat. Add new Berkeley DB handle methods to output debugging and performance tuning information to a C library FILE handle (C and C++ APIs only). \[#9204\]
2.  Fix a bug where db_stat could drop core if DB-\>open fails and no subdatabase was specified. \[#9273\]
3.  Add command-line arguments to the db_printlog utility to restrict the range of log file records that are displayed. \[#9307\]
4.  Fix a bug in the locking statistics where current locks included failed lock requests. \[#9314\]
5.  Fix a bug where db_archive would remove all log files when --enable-diagnostic and DB_NOTDURABLE were both specified. \[#9459\]
6.  Fix a bug where db_dump with the -r flag would output extra copies of the subdatabase information. \[#9808\]
7.  Fix a bug in db_archive that would cause log file corruption if the application had configured the environment with DB_PRIVATE. \[#9841\]
8.  Add support in db_load for resetting database LSNs and file IDs without having to reload the database. \[#9916\]
9.  Change the DB-\>stat() method to take a transaction handle as an argument, allowing DB-\>stat() to be called from within a transaction. \[#9920\]
10. Fix a bug in db_printlog where only the first filewould be displayed for in-memory logs. \[#11505\]
11. Fix a bug that prevented database salvage from working in Berkeley DB 4.3.21. \[#11649\]
12. Fix a bug in db_load which made it impossible to specify more than a single option on the command line. \[#11676\]

### Configuration, Documentation, Portability and Build Changes:

1.  Add pread and pwrite to the list of system calls applications can replace at run-time. \[#8954\]
2.  Add support for UTF-8 encoding of filenames on Windows. \[#9122\]
3.  Remove C++ dependency on snprintf. Compilers on HPUX 10.20 are missing header file support for snprintf(). \[#9284\]
4.  Change Berkeley DB to not use the open system call flag O_DIRECT, unless DB configured using --enable-o_direct. \[#9298\]
5.  Fix several problems with mutex alignment on HP/UX 10.20. \[#9404\]
6.  Fix a memory leak when HAVE_MUTEX_SYSTEM_RESOURCES is enabled. \[#9546\]
7.  Fix a bug in the sec002.tcl test for binary data. \[#9626\]
8.  Fix a bug where filesystem blocks were not being zeroed out in the On-Time embedded Windows OS. \[#9640\]
9.  Fix build problems with the Java API in Visual Studio .NET 2003. \[#9701\]
10. Add support for the gcc compiler on the Opteron platform. \[#9725\]
11. Add support for the small_footprint build option for VxWorks. \[#9820\]
12. Add support for linking of DLLs with MinGW. \[#9957\]
13. Remove the make target which builds the RPM package from the Berkeley DB distribution. \[#10233\]
14. Add a C++/XML example for ex_repquote. \[#10380\]
15. Fix a bug to link with lrt only if detected by configure (Mac OS X issue). \[#10418\]
16. Fix a bug and link Java and Tcl shared libraries with lpthread if required, for mutexes. \[#10418\]
17. Add support for building Berkeley DB on the HP NonStop OSS (Tandem) platform. \[10483\]
18. Change Berkeley DB to ignore EAGAIN from system calls. This fixed problems on NFS mounted databases. \[#10531\]
19. Remove a line with bt_compare and bt_prefix from the db_dump recovery test suite, which can cause failures on OpenBSD. \[#10567\]
20. Fix a conflict with the lock_init for building Berkeley DB on Cygwin. \[#10582\]
21. Add Unicode support for the Berkeley DB Windows API. \[#10598\]
22. Add support for 64-bit builds on Windows. \[#10664\]
23. Libtool version is now 1.5.8. \[#10950\]
24. Remove mt compilation flag for HP-UX 11.0. \[#11427\]
25. Fix a bug to link with lrt on Solaris to support fdatasync. \[#11437\]
