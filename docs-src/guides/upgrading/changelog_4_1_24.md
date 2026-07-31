---
title: "Berkeley DB 4.1.24 and 4.1.25 Change Log"
api-name: "Berkeley DB 4.1.24 and 4.1.25 Change Log"
source: docs/upgrading/changelog_4_1_24.html
---
## Berkeley DB 4.1.24 and 4.1.25 Change Log

<span class="sect2"> [Database or Log File On-Disk Format Changes:](changelog_4_1_24.md#idp50963888) </span>

<span class="sect2"> [Major New Features:](changelog_4_1_24.md#idp50959088) </span>

<span class="sect2"> [General Environment Changes:](changelog_4_1_24.md#idp50962280) </span>

<span class="sect2"> [General Access Method Changes:](changelog_4_1_24.md#idp50961984) </span>

<span class="sect2"> [Btree Access Method Changes:](changelog_4_1_24.md#idp50964272) </span>

<span class="sect2"> [Hash Access Method Changes:](changelog_4_1_24.md#idp50967400) </span>

<span class="sect2"> [Queue Access Method Changes:](changelog_4_1_24.md#idp50969240) </span>

<span class="sect2"> [Recno Access Method Changes:](changelog_4_1_24.md#idp50972088) </span>

<span class="sect2"> [C++-specific API Changes:](changelog_4_1_24.md#idp50973928) </span>

<span class="sect2"> [Java-specific API Changes:](changelog_4_1_24.md#idp50975768) </span>

<span class="sect2"> [Tcl-specific API Changes:](changelog_4_1_24.md#idp50950328) </span>

<span class="sect2"> [RPC-specific Client/Server Changes:](changelog_4_1_24.md#idp50958680) </span>

<span class="sect2"> [Replication Changes:](changelog_4_1_24.md#idp50977144) </span>

<span class="sect2"> [XA Resource Manager Changes:](changelog_4_1_24.md#idp50964336) </span>

<span class="sect2"> [Locking Subsystem Changes:](changelog_4_1_24.md#idp50987264) </span>

<span class="sect2"> [Logging Subsystem Changes:](changelog_4_1_24.md#idp50989192) </span>

<span class="sect2"> [Memory Pool Subsystem Changes:](changelog_4_1_24.md#idp50992072) </span>

<span class="sect2"> [Transaction Subsystem Changes:](changelog_4_1_24.md#idp50993160) </span>

<span class="sect2"> [Utility Changes:](changelog_4_1_24.md#idp50994744) </span>

<span class="sect2"> [Configuration, Documentation, Portability and Build Changes:](changelog_4_1_24.md#idp50997648) </span>

### Database or Log File On-Disk Format Changes:

1.  All of the access method database formats changed in the Berkeley DB 4.1 release (Btree/Recno: version 8 to version 9, Hash: version 7 to version 8, and Queue: version 3 to version 4). <span class="emphasis">*The format changes are entirely backward-compatible, and no database upgrades are needed.*</span>

### Major New Features:

1.  Berkeley DB now includes support for database encryption using the AES encryption standard. \[#1797\]
2.  Berkeley DB now includes support for database page checksums to allow detection of database corruption during I/O. \[#1797\]
3.  The shared memory buffer pool code base was substantially reworked in the 4.1 release to improve concurrent throughput. \[#4655\]

### General Environment Changes:

1.  Allow applications to specify transaction handles to the DB-\>open method call, so database creation can be grouped with other Berkeley DB calls in a single transaction. \[#4257\]
2.  Add the DB_ENV-\>remove and DB_ENV-\>rename method calls that support transactional protection of database removal and renaming. \[#4257\]
3.  Add the DB_ENV-\>set_flags flags DB_DIRECT_DB and DB_DIRECT_LOG, which disable the system's buffer cache where possible. \[#4526\]
4.  Unlock the pthread mutex if pthread_cond_wait() returns an error. \[#4872\]
5.  Fix a memory leak caused by running recovery. \[#4913\]
6.  Fix a bug in which closing an environment with open database handles could result in application crashes. \[#4991\]
7.  Fix a bug where DB_CONFIG files were ignored if the database environment defaulted to the application's current working directory. \[#5265\]
8.  Fix a bug where transaction abort or commit could fail to destroy the handle. \[#5633\]
9.  Fix a set of bugs where the Berkeley DB API could return DB_RUNRECOVERY without panic-ing the database environment itself or calling the application's panic-callback function. \[#5743\]
10. Fix a bug in where DB=\>rename and DB-\>remove method calls could leak a transaction and its locks. \[#5824\]
11. Fix a bug where recovery feedback could return values greater than 100. \[#6193\]
12. Fix a bug where a page allocated by a transaction, eventually aborted because of application or system failure, could appear twice in the free list, if catastrophic recovery was performed. \[#6222\]
13. Add a new flag, DB_AUTO_COMMIT, that wraps all database modification operations inside a transaction, to the DB_ENV-\>set_flags method. \[#6395\]
14. Fix a bug where recovery could fail when upgrading between releases. \[#6372\]
15. Fix a recovery bug where pages that were repeatedly freed and allocated could be lost. \[#6479\] \[#6501\]
16. Change DB_CONFIG reading to handle non-\<newline\> terminated last line. \[#6490\]

### General Access Method Changes:

1.  Allow applications to specify transaction handles to the DB-\>associate method call, so secondary index creation can be grouped with other Berkeley DB calls in a single transaction. \[#4185\]
2.  Add a new flag, DB_AUTO_COMMIT, that wraps single database operations inside a transaction. This flag is supported by the DB-\>del, DB-\>open, DB-\>put, DB-\>truncate,DB_ENV-\>remove, and DB_ENV-\>rename methods. \[#4257\]
3.  The DB_EXCL DB-\>open method flag has been enhanced to work on subdatabases. \[#4257\]
4.  Fix a bug in which a DB-\>put(DB_APPEND) could result in leaked memory or a corruption in the returned record number. \[#5002\]
5.  Fix a bug in the database salvage code that could leave pages pinned in the cache. \[#5037\]
6.  Add a flag to the DB-\>verify method to output salvaged key/data pairs in printable characters. \[#5037\]
7.  Fix a bug in which DB-\>verify() might continue and report extraneous database corruption after a fatal error. \[#5131\]
8.  Fix a bug where calling the DB-\>stat method before the DB-\>open method could drop core. \[#5190\]
9.  Fix a bug in which a DB-\>get, DBcursor-\>c_get, or DBcursor-\>c_pget on a secondary index, in the Concurrent Data Store product, could result in a deadlock. \[#5192\]
10. Fix a bug in which DB-\>verify() could correctly report errors but still return success. \[#5297\]
11. Add support for the DB-\>set_cache_priority interface, that allows applications to set the underlying cache priority for their database files. \[#5375\]
12. Fix a bug where calling DBcursor-\>c_pget with a database that is not a secondary index would drop core. \[#5391\]
13. Fix a bug where a bug in the DB-\>truncate method could cause recovery to fail. \[#5679\]
14. Fix a bug where DB_GET_RECNO would fail if specified to a secondary index. \[#5811\]
15. Fix a bug where building a secondary index for an existing primary database could fail in Concurrent Data Store environments. \[#5811\]
16. Fix a bug where the DB-\>rename method could fail, causing a problem during recovery. \[#5893\]
17. Fix a bug in which a DB-\>get or DB-\>pget call on a secondary index could fail when done with a handle shared among multiple threads. \[#5899\]
18. Fix a bug in which a DB-\>put operation on a database with off-page duplicates could leak a duplicate cursor, thereby preventing transactions being able to commit. \[#5936\]
19. Fix a bug where overflow page reference counts were not properly maintained when databases were truncated. \[#6168\]
20. Fix a bug where the bulk get APIs could allocate large amounts of heap memory. \[#6439\] \[#6520\]

### Btree Access Method Changes:

1.  Fix a bug that prevented loads of sorted data, with duplicates at the end of the tree, from creating compact trees. \[#4926\]
2.  No longer return a copy of the key if the DB_GET_BOTH or DB_GET_BOTH_RANGE flags are specified. \[#4470\]
3.  Fix a bug where the fast-search code could hold an unlocked reference to a page, which could lead to recovery failure. \[#5518\]
4.  Fix a bug where some cursor operations on a database, for which the bt_minkey size had been specified, could fail to use the correct overflow key/data item size. \[#6183\]
5.  Fix a bug where the recovery of an aborted transaction that did a reverse Btree split might leave a page in an inconsistent state. \[#6393\]

### Hash Access Method Changes:

1.  Fix bugs that could cause hash recovery to drop core. \[#4978\]
2.  Use access method flags instead of interface flags to check for read-only access to a hash database with an application-specified hash function. \[#5121\]
3.  Fix a bug where a hash database allocation of a new set of buckets may be improperly recovered by catastrophic recovery if the transaction is split across log files and the beginning segment of the transaction is not included in the set of logs to be recovered. \[#5942\]
4.  Fix a bug where aborting particular hash allocations could lead to a database on which the verifier would loop infinitely. \[#5966\]
5.  Fix a bug where a memory allocation failure could result in a system hang. \[#5988\]
6.  Remove nelem from the Hash access method statistics (the value was incorrect once items had been added or removed from the database). \[#6101\]
7.  Fix a bug where a page allocated by an aborted transaction might not be placed on the free list by recovery, if the file holding the page was created as part of recovery, and a later page was part of a hash bucket allocation. \[#6184\]
8.  Fix a bug where allocated pages could be improperly recovered on systems that require explicit zero-ing of filesystem pages. \[#6534\]

### Queue Access Method Changes:

1.  No longer return a copy of the key if the DB_SET_RANGE flag is specified. \[#4470\]
2.  Fix a bug where DBcursor-\>c_get (with DB_MULTIPLE or DB_MULTIPLE_KEY specified) could fail on a Queue database if the record numbers had wrapped. \[#6397\]

### Recno Access Method Changes:

1.  No longer return a copy of the key if the DB_GET_BOTH or DB_GET_BOTH_RANGE flags are specified. \[#4470\]
2.  Fix a bug where non-transactional locking applications could leak locks when modifying Recno databases. \[#5766\]
3.  Fix a bug where DBcursor-\>c_get with the DB_GET_RECNO flag would panic the environment if the cursor was uninitialized. \[#5935\]
4.  Fix a bug where deleting pages from a three-level Recno tree could cause the database environment to panic. \[#6232\]

### C++-specific API Changes:

1.  C++ DbLock::put is replaced by DbEnv::lock_put to match the C and Java API change in Release 4.0. \[#5170\]
2.  Declared destructors and methods within Db and DbEnv classes to be virtual, making subclassing safer. \[#5264\]
3.  Fixed a bug where Dbt objects with no flags set would not be filled with data by some operations. \[#5706\]
4.  Added DbDeadlockException, DbRunRecoveryException, and DbLockNotGrantedException classes to C++, and throw them accordingly. \[#6134\]
5.  Added C++ methods to support remaining conversions between C++ classes and C structs where appropriate. In particular, DbTxn/DB_TXN conversions and DbMpoolFile/DB_MPOOLFILE were added. \[#6278\]
6.  Fix a bug in DbEnv::~DbEnv() that could cause memory corruption if a DbEnv was deleted without being closed. \[#6342\]
7.  Reordered C++ class declarations to avoid a GCC g++ warning about function inlining. \[#6406\]
8.  Fix a bug in the DbEnv destructor that could cause memory corruption when an environment was destroyed without closing first. \[#6342\]
9.  Change DbEnv and Db destructor behavior to close the handle if it was not already closed. \[#6342\]

### Java-specific API Changes:

1.  Added check for system property "sleepycat.Berkeley DB.libfile" that can be used to specify a complete pathname for the JNI shared library. This is needed as a workaround on Mac OS X, where libtool cannot currently create a library with a .jnilib extension which is what the current JDK expects by default. \[#5664\]
2.  Fixed handling of JVM out of memory conditions, when some JNI methods return NULL. When the JVM runs out of memory, calls should consistently fail with OutOfMemoryErrors. \[#5995\]
3.  Added Dbt.get_object and Dbt.set_object convenience routines to the Java API to make using serialization easier. \[#6113\]
4.  Fixed a bug that prevented Java's Db.set_feedback from working, fixed document for Java's Db.set_feedback, some callback methods were misnamed. \[#6137\]
5.  Fix a NullPointerException in Db.finalize() if the database had been closed. \[#6504\]
6.  Marked DbEnv constructor with "throws DbException". \[#6342\]

### Tcl-specific API Changes:

None.

### RPC-specific Client/Server Changes:

1.  Fix a bug where Db and DbEnv handles were not thread-safe. \[#6102\]

### Replication Changes:

1.  A large number of replication bugs were fixed in this release. The replication support is now believed to be production quality.
2.  Add the DB_ENV-\>set_rep_limit interface, allowing applications to limit the data sent in response to a single DB_ENV-\>rep_process_message call. \[#5999\]
3.  Add the DB_ENV-\>set_rep_stat interface, returning information from the replication subsystem \[#5919\]

### XA Resource Manager Changes:

1.  Added support for multithreaded XA. Environments can now have multiple XA transactions active. db_env_xa_attach() can be used to get a DB_TXN that corresponds to the XA transaction in the current thread. \[#5049\]
2.  Added a com.sleepycat.Berkeley DB.xa package that implements J2EE support for XA. This includes new DbXAResource, DbXid classes that implement the XAResource and Xid interfaces. \[#5049\]
3.  Fix a bug where aborting a prepared transaction after recovery may fail. \[#6383\]
4.  Fix a bug where recovery might fail if a prepared transaction had previously extended the size of a file and then was aborted. \[#6387\]
5.  Fix a bug where if the commit of a prepared transaction fails the transaction would be aborted. \[#6389\]

### Locking Subsystem Changes:

1.  Fix a bug where lock counts were incorrect if a lock request returned DB_LOCK_NOTGRANTED or an error occurred. \[#4923\]
2.  Fix a bug where lock downgrades were counted as releases, so the lock release statistics could be wrong. \[#5762\]
3.  Fix a bug where the lock and transaction timeout values could not be reset by threads of control joining Berkeley DB database environments. \[#5996\]
4.  Fix a bug where applications using lock and/or transaction timeouts could hit a race condition that would lead to a segmentation fault. \[#6061\]

### Logging Subsystem Changes:

1.  DB_ENV-\>log_register and DB_ENV-\>log_unregister have been removed from the interface. \[#0046\]
2.  Fix a bug where creating a database environment with a nonexistent logging directory could drop core. \[#5833\]
3.  Add support allowing applications to change the log file size in existing database environments. \[#4875\]
4.  Fix a bug where a write error on a log record spanning a buffer could cause transaction abort to fail and the database environment to panic. \[#5830\]

### Memory Pool Subsystem Changes:

1.  The DB_INCOMPLETE error has been removed, as cache flushing can no longer return without completing. \[#4655\]
2.  Fix a bug where Berkeley DB might refuse to open a file if the open was attempted while another thread was writing a large buffer. \[#4885\]
3.  Prefer clean buffers to dirty buffers when selecting a buffer for eviction. \[#4934\]
4.  Fix a bug where transaction checkpoint might miss flushing a buffer to disk. \[#5033\]
5.  Fix a bug where Berkeley DB applications could run out of file descriptors. \[#5535\]
6.  Fix bugs where Berkeley DB could self-deadlock on systems requiring mutex resource reclamation after application failure. \[#5722\] \[#6523\]

### Transaction Subsystem Changes:

1.  Go back only one checkpoint, not two, when performing normal recovery. \[#4284\]
2.  Fix a bug where an abort of a transaction could fail if there was no disk space for the log. \[#5740\]
3.  Fix a bug where the checkpoint log-sequence-number could reference a nonexistent log record. \[#5789\]
4.  Fix a bug where subtransactions which allocated pages from the filesystem and subsequently aborted could cause other pages allocated by sibling transactions to not be freed if the parent transaction then aborted. \[#5903\]
5.  Fix a bug where transactions doing multiple updates to a queue database which spanned a checkpoint could be improperly handled by recovery. \[#5898\]

### Utility Changes:

1.  Fix a bug where the -p option could not be specified with the -R or -r options. \[#5037\]
2.  The utilities were modified to correctly size their private caches in order to handle databases with large page sizes. \[#5055\]
3.  Fix a bug in which utilities run with the -N option would fail to ignore the environment's panic flag. \[#5082\]
4.  Fix a bug where invalid log records could cause db_printlog to drop core. \[#5173\]
5.  Add a new option to the db_verify utility to support verification of files that include databases having non-standard sorting or hash functions. \[#5237\]

### Configuration, Documentation, Portability and Build Changes:

1.  Replace test-and-set mutexes on Windows with a new mutex implementation that signals an event to wake blocked threads. \[#4413\]
2.  Support configuration of POSIX pthread mutexes on systems where the pthread mutexes do not support inter-process locks. \[#4942\]
3.  Add mutex support for the ARM architecture using the gcc compiler. \[#5018\]
4.  On Windows NT/2000/XP, switched to atomic seek-and-read/write operations to improve performance of concurrent reads \[#0654\].
5.  Support cross-compilation using the GNU compiler tool chain. \[#4558\]
6.  Fix a bug where libraries were always installed read-only. \[#5096\]
7.  Fix a bug where temporary files on VxWorks could fail. \[#5160\]
8.  Fix a bug where Berkeley DB did not install correctly if the system cp utility did not support the -f option. \[#5111\]
9.  Correct the documentation for the Queue access method statistics field qs_cur_recno to be the "Next available record number". \[#5190\]
10. Fix a bug where file rename could fail on Windows/9X. \[#5223\]
11. Removed support for Microsoft Visual Studio 5.0 \[#5231\]
12. Switched to using HANDLEs for all I/O operations on Windows to overcome a hard limit of 2048 open file descriptors in Microsoft's C runtime library. \[#5249\]
13. Fix a bug where Berkeley DB error message routines could drop core on the PowerPC and UltraSPARC architectures. \[#5331\]
14. Rename OSTREAMCLASS to \_\_DB_OSTREAMCLASS in db_cxx.h to avoid stepping on application name space. \[#5402\]
15. Support Linux on the S/390 architecture. \[#5608\]
16. Work around a bug in Solaris where the pthread_cond_wait call could return because a signal was delivered to the application. \[#5640\]
17. Fix build line for loadable libraries to include -module to support Mac OS X. \[#5664\]
18. Fix a bug in the PPC mutex support for the Mac OS X system. \[#5781\]
19. Added support for Java on Mac OS X. A workaround on the Java command line is currently necessary; it is documented. \[#5664\]
20. Added support for Tcl on Mac OS X. \[#5664\]
21. Update Windows build instructions to cover Visual C++ .NET. \[#5684\]
22. AIX configuration changes for building on AIX 4.3.3 and 5 with both standard and Visual Age compilers. \[#5779\]
23. Add a new UNIX configuration argument, --with-mutex=MUTEX, to allow applications to select a mutex implementation. \[#6040\]
24. Changed libtool and configure so we can now correctly build and install Tcl and Java loadable shared libraries that work on Mac OS X. \[#6117\]
25. Fix mutex alignment problems on historic HP-UX releases that could make multiprocess applications fail. \[#6250\]
26. Installed static .a archives on Mac OS X need to be built with the ranlib -c option so linked applications will not see undefined \_\_db_jump errors. \[#6215\]
27. Upgrade pthread and mmap support in the uClibc library to support Berkeley DB. \[#6268\]
28. Fixed error in determining include directories during configuration for --enable-java. The error can cause compilation errors on certain systems with newer versions of gcc. \[#6445\]
