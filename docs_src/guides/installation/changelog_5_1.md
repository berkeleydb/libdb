---
title: "Berkeley DB Library Version 11.2.5.1 Change Log"
api-name: "Berkeley DB Library Version 11.2.5.1 Change Log"
source: docs/installation/changelog_5_1.html
---
## Berkeley DB Library Version 11.2.5.1 Change Log

<span class="sect2"> [Database or Log File On-Disk Format Changes](changelog_5_1.md#idp1052992) </span>

<span class="sect2"> [New Features](changelog_5_1.md#idp953176) </span>

<span class="sect2"> [Database Environment Changes](changelog_5_1.md#idp1045336) </span>

<span class="sect2"> [Concurrent Data Store Changes](changelog_5_1.md#idp1059760) </span>

<span class="sect2"> [Access Method Changes](changelog_5_1.md#idp981016) </span>

<span class="sect2"> [API Changes](changelog_5_1.md#idp1049008) </span>

<span class="sect2"> [SQL-Specific API Changes](changelog_5_1.md#idp1055592) </span>

<span class="sect2"> [Tcl-Specific API Changes](changelog_5_1.md#idp1056952) </span>

<span class="sect2"> [Java-Specific API Changes](changelog_5_1.md#idp1052280) </span>

<span class="sect2"> [C#-Specific API Changes](changelog_5_1.md#idp987592) </span>

<span class="sect2"> [Direct Persistence Layer (DPL), Bindings and Collections API](changelog_5_1.md#idp1060648) </span>

<span class="sect2"> [Replication Changes](changelog_5_1.md#idp1070000) </span>

<span class="sect2"> [Locking Subsystem Changes](changelog_5_1.md#idp1080936) </span>

<span class="sect2"> [Logging Subsystem Changes](changelog_5_1.md#idp1092608) </span>

<span class="sect2"> [Memory Pool Subsystem Changes](changelog_5_1.md#idp1076376) </span>

<span class="sect2"> [Mutex Subsystem Changes](changelog_5_1.md#idp1080752) </span>

<span class="sect2"> [Transaction Subsystem Changes](changelog_5_1.md#idp1089584) </span>

<span class="sect2"> [Test Suite Changes](changelog_5_1.md#idp1067160) </span>

<span class="sect2"> [Utility Changes](changelog_5_1.md#idp1088000) </span>

<span class="sect2"> [Configuration, Documentation, Sample Apps, Portability, and Build Changes](changelog_5_1.md#idp1091312) </span>

<span class="sect2"> [Example Changes](changelog_5_1.md#idp1081576) </span>

<span class="sect2"> [Miscellaneous Bug Fixes](changelog_5_1.md#idp1102152) </span>

<span class="sect2"> [Deprecated Features](changelog_5_1.md#idp1100024) </span>

<span class="sect2"> [Known Bugs](changelog_5_1.md#idp1100672) </span>

This is the changelog for Berkeley DB 11<span class="emphasis">*g*</span> Release 2 (library version 11.2.5.1).

### Database or Log File On-Disk Format Changes

1.  The database file format was unchanged in 11gR2 library version 11.2.5.1.

2.  The log file format was unchanged in 11gR2 library version 11.2.5.1.

### New Features

1.  Added Performance event monitoring support for DTrace and SystemTap which can be enabled during configuration. Static probes have been defined where statistics values are updated, where mutex or transactional consistency lock waits occur, and where some other potentially lengthy operations may be initiated. \[#15605\]

2.  Added a new acknowledge policy - DB_REPMGR_ACKS_ALL_AVAILABLE. \[#16762\]

3.  Added transactional bulk loading optimization for non-nested transactions. \[#17669\]

4.  Added exclusive transaction support for the SQL API. \[#17822\]

5.  Added support for bulk update and delete in C# API. \[#18011\]

6.  Added a db_replicate utility. \[#18326\]

7.  Added an implementation of the Online Backup API. \[#18500\]

8.  Added support in Berkeley DB SQL for the vacuum and incremental vacuum pragmas \[#18545\]

9.  Added an option to automatically convert SQLite databases to Berkeley DB on opening. \[#18531\]

10. Added BDBSQL_SHARE_PRIVATE, an option to enable inter-process sharing of DB_PRIVATE environments using multiple-reader. \[#18533\]

11. Added database-level locking to optimize single-threaded operations and remove locking limitations for database load operations. \[#18549\]

12. Added support for DB_INIT_REP, DB_PRIVATE and DB_THREAD in DB_CONFIG file.\[#18555\]

13. Added support for the BDBSQL_DEFAULT_PAGE_SIZE pragma to override Berkeley DB's choice of page size depending on the filesystem. Use SQLITE_DEFAULT_PAGE_SIZE rather than a hard-coded default. \[#18577\]

14. Added an extension that allows access to binary files stored outside of the database. What is stored in the database is a pointer to the binary file. \[#18635\]

15. Added .stat command to dbsql shell to print environment, table, and index statistics. \[#18640\]

16. Added enhancements to reduce the size of indexes in the SQL API by allowing duplicates in the index database and moving the rowid from the index key into the index data. \[#18653\]

17. Added a compile time flag BDBSQL_FILE_PER_TABLE that causes each table to be created in a separate file. This flag replaces the BDBSQL_SPLIT_META_TABLE flag. \[#18664\]

18. Added the handling of read only and read write open of the same database in BDB SQL \[#18672\]

19. Added an encryption implementation to the SQL API \[#18683\]

### Database Environment Changes

1.  Fixed failchk behavior on QNX. \[#17403\]

2.  Fixed a bug that prevented the same process from connecting to the database after recovery is performed. \[#18535\]

3.  Fixed a bug which would occur when recovery checkpoint was not written because the cache ran out of space attempting to flush the memory pool cache. The environment would be recovered and all database where made available, but some databases would incorrectly closed. This would cause a subsequent recovery to fail on its backward pass with the error "PANIC: No such file or directory". \[#18590\]

4.  Fixed a bug that could cause recovery to fail with the error "DB_LOGC-\>get: log record LSN %u/%u: checksum mismatch" if the last log file was nearly full and ended with a partially written log record which was smaller than a checkpoint record. It now erases the invalid partial record before switching to the new log file. \[#18651\]

### Concurrent Data Store Changes

1.  None

### Access Method Changes

1.  Fixed a bug such that segementation fault does not occur if DB-\>set_partition_dirs is called before DB-\>set_partition. \[#18591\]

2.  Fixed a bug such that the error "unknown path" does not occur if you put duplicate records into a duplicated sorted HASH database with DB_OVERWRITE_DUP option. \[#18607\]

3.  Added the ability to specify that data should not be logged when removing pages from a database. This can be used if the ability to recover the data is not required after the database has been removed. \[#18666\]

4.  Fixed a bug that caused an aborting transaction to fail if it aborted while a DB-\>compact of the same HASH database was compacting the dynamic hash table \[#18695\]

5.  Fixed a bug that could cause DB-\>compact to loop on a DB_RECNO database or a database with an multilevel unsorted off page duplicate tree. \[#18722\]

6.  Fixed a bug that could cause an illegal page type error when using a HASH database with MVCC and the HASH table was contracted and then extended. \[#18785\]

7.  Fixed locking bugs: \[#18789\] The Db-\>compact method of BTREE with MVCC would return an unpinned page. The RECNO option would fail to lock the next page when splitting a leaf page.

8.  Fixed a bug that could cause data to not be returned in a HASH database that was one of multiple databases in a file and was opened prior to running DB-\>compact method on that database in another thread of control \[#18824\]

9.  Fixed a bug where doing a bulk insert with secondaries could return an error incorrectly. \[#18878\]

10. Fixed a bug that would return DB_NOTFOUND instead of DB_BUFFER_SMALL when the first item in a HASH database is larger than the user supplied buffer. \[#18829\]

### API Changes

1.  Fixed various items uncovered by extending DB_CONFIG support: \[#18720\] - Added missing set_cache_max method, and fixed name of log_set_config (was set_log_config). - Added new DB_ENV-\>repmgr_get_local_site method. - Fixed a bug which could fail to allocate enough mutexes when specifying a maximum cache size. - Fixed a bug that could allocate multiple caches when a small cache size was specified.

### SQL-Specific API Changes

1.  Allowed SQL applications to attach to the same database multiple times unless shared cache mode is explicitly requested. \[#18340\]

2.  Fixed a bug where auto-removal of log files after writing a checkpoint was not functioning correctly. \[#18413\]

3.  Fixed a race between opening and closing SQL databases from multiple threads that could lead to the error "DB_REGISTER limits processes to one open DB_ENV handle per environment". \[#18538\]

4.  Optimized the SQL adapter for joins. Reduce the number of Berkeley DB operations in a join by caching the maximum key in the primary. \[#18566\]

5.  A SQLITE_LOCKED or SQLITE_BUSY error returned by a statement in an explict transaction will no longer invalidate the entire transaction, but just the statement that returned the error. \[#18582\]

6.  Changed how multiple connections to the same database are detected. Used a fileid so that different paths can be used without error. \[#18646\]

7.  Fixed a bug where the journal (environment) directory was being created prior to the actual environment. \[#18656\]

8.  Added a new PRAGMA to allow tuning of when checkpoints are run. \[#18657\]

9.  Fixed spurious "column \<x\> not unique" error messages.\[#18667\]

10. Fixed a segmentation fault that could happen when memory could not be allocated for the index key in the SQL API.\[#18783\]

11. Fixed a bug causing a segfault when releasing a savepoint that was already released. \[#18784\]

### Tcl-Specific API Changes

1.  Changed to link tcl8.5 by default on Windows\[#18244\]

### Java-Specific API Changes

1.  Fixed a bug where getAllowPopulate and getImmutableSecondaryKey method always returned false for SecondaryConfig objects returned by SecondaryDatabase.getSecondaryConfig method. \[#16018\]

2.  Fixed a bug which made it impossible to (re)set VerboseConfig.REPLICATION_SYSTEM on the Java API. \[#17561\]

3.  Fixed a bug where populating a SecondaryDatabase on open could lead to an OutOfMemoryException. \[#18529\]

4.  Fixed a bug such that segementation fault does not occur when putting records into callback-partitioned database. \[#18596\]

5.  Fixed a bug where DatabaseConfig.getUnsortedDuplicates method returned true when the datbase had been configured for sorted duplicates. \[#18612\]

6.  Initialized DatabaseConfig.pageSize so that it can be queried. \[#18691\]

7.  Fixed a bug by opening a write cursor for Direct Persistent Layer(DPL) entity's put operation in the Concurrent Data Store product. \[#18692\]

8.  Synchronized Java persistence code and tests from Java Edition to Berkeley DB. \[#18711\]

9.  Introduced the EnvironmentConfig.setReplicationInMemory method as a way to configure in-memory internal replication files before opening the Environment handle on the Java API. \[#18719\]

10. Fixed a bug in the bulk DatabaseEntry class, where it was possible to overflow the buffer. \[#18850\]

11. Added LEASE_TIMEOUT field to the ReplicationTimeoutType class that enables configuring the amount of time a client grants its Master Lease to a master. \[#18867\]

### C#-Specific API Changes

1.  Fixed a bug in BTree prefix comparison method such that there is no problem when the application needs to save a number larger than or equal to 2^31. The BTree prefix comparison function now returns an unsigned int instead of a signed int. \[#18481\]

2.  Fixed a bug which caused the HasMultiple method to throw an exception when there were multiple databases in a single database file. \[#18483\]

3.  Fixed a bug to ensure the CachePriority is set for Database and Cursor objects. \[#18716\]

4.  Fixed a bug that use leading to the error: "Transaction that opened the DB handle is still active" when applications used different transactional handles in the associate and open methods in a secondary database. \[#18873\]

### Direct Persistence Layer (DPL), Bindings and Collections API

1.  All setter methods in the DPL \|StoreConfig\| and \|EvolveConfig\| now return \|this\| rather than having a \|void\| return type. This change requires that applications using the DPL be recompiled. \[#17021\]

2.  Improve performance of \|StoredCollection.removeAll\|. This method no longer iterates over all records in the stored collection. \[#17727\]

3.  Several new tuple formats and binding classes have been added in the \|com.sleepycat.bind.tuple\| package:

    - Packed integer formats have been added that support default natural sorting. These are intended to replace the old unsorted packed integer formats.
    - Two new \|BigDecimal\| formats have been added. One format supports default natural sorting. The other format is unsorted, but has other advantages: trailing zeros after the decimal place are preserved, and a more compact, faster serialization format is used. See the \|com.sleepycat.bind.tuple\| package description for an overview of the new bindings and a comparative description of all tuple bindings. \[#18379\]

4.  The following classes are now certified to be serializable. \[#18738\]

    - com.sleepycat.persist.IndexNotAvailableException
    - com.sleepycat.persist.StoreExistsException
    - com.sleepycat.persist.StoreNotFoundException
    - com.sleepycat.persist.evolve.DeletedClassException
    - com.sleepycat.persist.evolve.IncompatibleClassException

### Replication Changes

1.  Replication Manager now uses the standard system implementation of getaddrinfo() when running on Windows, which means that it can support IPv6 addresses if support is present and configured in the operating system. \[#18263\]

2.  Fixed a bug which caused a "full election" to fail if a majority of sites were not ready when the election started. \[#18456\]

3.  Fixed a bug which could occur when using bulk transfer with Replication Manager. When closing a DB_ENV handle, any remaining bulk buffer contents are flushed, and Replication Manager could have tried to send the resulting messages even though its connections had already been closed, leading in rare circumstances to spurious EBADF error reports, or possibly even arbitrary memory corruption. \[#18469\]

4.  Fixed a bug which caused Replication Manager to wait for acknowledgement from client, even if it had failed to send a log record, due to "queue limit exceeded". Replication Manager now returns immediately, with a PERM_FAILED indication, to avoid a pointless delay to the commit() operation. \[#18682\]

5.  Fixed a bug where changes made in one process to Replication Manager configuration values (such as ack policy or ack timeout) were not observed in other processes sharing the same database environment. \[#18839\]

6.  Fixed bugs that could prevent client synchronization from completing due to a failure to request missing log records. \[#18849\]

7.  Fixed a bug where a client that had rolled back transactions to synchronize with a new master, failed to invalidate existing database handles later used for cursor operations based on an explicitly provided transaction. \[#18862\]

8.  Fixed a bug where Replication Manager called for an election after a DUPMASTER event, even when using Master Leases. In such a case it now simply accepts the new (remote) master. \[#18864\]

9.  Fixed a bug which would cause failure if client env attempted to perform sync-up recovery to a point in the log that happened to fall exactly on a log file boundary. \[#18907\]

### Locking Subsystem Changes

1.  Moved the wait mutex from the lock structure to the locker structure, reducing the number of mutexes required in the system. \[#18685\]

### Logging Subsystem Changes

1.  None.

### Memory Pool Subsystem Changes

1.  Fixed a race condition that was causing the error: "Unable to allocate space from the buffer cache". The error can only be triggered when multiple memory pool regions are used and there is a periodic gathering and clearing of statistics. This also fixes a second bug where if you compile without statistics and explicitly set the memoru pool default pagesize, other environment handles to that environment would not see the correct memory pool default pagesize. \[#18386\]

2.  Fixed a bug where the get_cachesize method and the mpool_stat method returned the initial cache size, even if the cache size had been changed. \[#18706\]

3.  Changed memory pool allocation so that the EIO error is returned rather than the ENOMEM error when the memory cannot be allocated because dirty pages cannot be written. \[#18740\]

### Mutex Subsystem Changes

1.  Fixed problems with the printed statistics for DB_MUTEX_SHARED latches. The DB_STAT_CLEAR flag (as specified by db_stat -Z) did not clear the counts of the number of times a shared latch either had to wait to get access or was able to get the latch without waiting. Also, the ownership state of a test-and-set latch (not a hybrid one) was always displayed as not owned, even when it was held. \[#17585\] \[#18743\]

### Transaction Subsystem Changes

1.  Fixed bugs that could caused PANIC or DB_RUNRECOVERY errors when the synchronization of the transaction log failed. \[#18588\]

2.  Fix javadoc to note the exception to the rule that a transaction handle may not be accessed after commit() operaton (getCommitToken() is allowed). \[#18730\]

### Test Suite Changes

1.  None. \[#18831\]

### Utility Changes

1.  Modified the db_printlog and db_dump -da so that they use the same formatting. The db_logprint utility now uses the message stream. Both db_dump -da and db_printlog accept a -D flag to indicate the numer of bytes of data items to display. You can set this value when calling the DB_ENV-\>set_data_len method or in the DB_CONFIG. \[#18365\]

2.  Fixed a bug that caused a segmentation violation when using the db_printlog utility. \[#18694\]

3.  Fixed a bug in db_hotbackup that would cause a trap if the -D flag is used and the DB_CONFIG file does not specify a log directory. \[#18841\]

### Configuration, Documentation, Sample Apps, Portability, and Build Changes

1.  Added support and documentation for iPhone OS. \[#18223\]

2.  Fix a bug to make the configuration option --enable-debug work when CFLAGS is set. \[#18432\]

3.  Updated Visual Studio project files to enable ADO.NET support. \[#18448\]

4.  Enhanced the source tree layout making it easier to navigate. \[#18492\]

5.  Fixed the --enable-dbm argument to configure. \[#18497\]

6.  Fixed Visual Studio project files so that they can load into Visual Studio 2010. \[#18505\]

7.  Updated Windows CE build files to be consistent with desktop Windows build files. Added Windows Mobile 6.5.3 Professional as a target platform. \[#18516\]

8.  Added a fix so that an error message is displayed when the 'ar' utility is missing when configured. \[#18619\]

9.  Added tighter integration of JDBC on POSIX/autoconf by including an argument --enable-jdbc to configure. \[#18621\]

10. Fix build conflicts in log verify with other configurations. \[#18658\]

11. Upgraded Berkeley DB SQL to SQLite version 3.7.0 \[#18857\]

### Example Changes

1.  Renamed examples/c/bench_001 to examples/c/ex_bulk. \[#18537\]

### Miscellaneous Bug Fixes

1.  Provided a functionality on the Windows platform to choose a default page size based on the underlying file system sector size. \[#16538\]

2.  Changed DB_NOSYNC from an "operation" constant to a flag value. \[#17775\]

3.  Changed the default permissions for files in the release tree to allow write access. \[#17974\]

4.  Fixed a bug which caused database verification to hang when verifying a database in a Concurrent Data Store environment that performs locking on an environment-wide basis (DB_CDB_ALLDB.) \[#18571\]

### Deprecated Features

1.  \[#18871\] Removed the mod_db4 PHP/Apache wrapper. It only supported Apache 1.3 and has not been actively supported. Use php_db4 instead.

### Known Bugs

1.  None
