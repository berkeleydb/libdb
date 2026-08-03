---
title: "Berkeley DB Library Version 11.2.5.3 Change Log"
api-name: "Berkeley DB Library Version 11.2.5.3 Change Log"
source: docs/installation/changelog_5_3.html
---
## Berkeley DB Library Version 11.2.5.3 Change Log

<span class="sect2"> [Changes between 11.2.5.3.21 and 11.2.5.3.28](changelog_5_3.md#idp839120) </span>

<span class="sect2"> [Changes between 11.2.5.3.15 and 11.2.5.3.21](changelog_5_3.md#idp845408) </span>

<span class="sect2"> [Database or Log File On-Disk Format Changes](changelog_5_3.md#idp636088) </span>

<span class="sect2"> [New Features](changelog_5_3.md#idp856040) </span>

<span class="sect2"> [Database Environment Changes](changelog_5_3.md#idp853696) </span>

<span class="sect2"> [Access Method Changes](changelog_5_3.md#idp844240) </span>

<span class="sect2"> [SQL API Changes](changelog_5_3.md#idp838728) </span>

<span class="sect2"> [Java-specific API changes](changelog_5_3.md#idp863240) </span>

<span class="sect2"> [Replication Changes](changelog_5_3.md#idp867984) </span>

<span class="sect2"> [Locking Subsystem Changes](changelog_5_3.md#idp853912) </span>

<span class="sect2"> [Logging Subsystem Changes](changelog_5_3.md#idp844888) </span>

<span class="sect2"> [Memory Pool Subsystem Changes](changelog_5_3.md#idp868368) </span>

<span class="sect2"> [Mutex Subsystem Changes](changelog_5_3.md#idp883216) </span>

<span class="sect2"> [Transaction Subsystem Changes](changelog_5_3.md#idp875448) </span>

<span class="sect2"> [Utility Changes](changelog_5_3.md#idp889064) </span>

<span class="sect2"> [Configuration, Documentation, Sample Apps, Portability and Build Changes](changelog_5_3.md#idp892136) </span>

<span class="sect2"> [Known Bugs](changelog_5_3.md#idp892656) </span>

This is the changelog for Berkeley DB 11<span class="emphasis">*g*</span> Release 2 (library version 11.2.5.3).

### Changes between 11.2.5.3.21 and 11.2.5.3.28

1.  Fixed tcl library linking for AIX 7. \[#17109\]

2.  Fixed a bug that could cause a trap if a lock timeout occurred while opening a database. \[#21098\]

3.  Fixed missing encryption support for the Android JDBC driver. \[#21129\]

4.  Fixed an incorrect message being displayed when the -l option was specified to the db_hotbackup utility. \[#21313\]

5.  Fixed a bug that DB_ENV-\>log_get_config did not work correctly before DB_ENV-\>open. \[#21359\]

6.  Fixed a bug that could cause a SQL build failure when FTS3 is enabled. \[#21382\]

7.  Fixed a bug that prevented in-memory SQL database from being created properly. They can now be created without the use of the SQLITE_OPEN_CREATE flag. \[#21456\]

8.  Fixed a memory leak in SQL online backup. \[#21460\]

9.  Added additional examples for C++ \[#21477\]

10. Fixed a bug that could make odbc fail to build with sqlite source code. \[#21490\]

11. Fixed bugs in compaction of large keys in the upper levels of btrees. \[#21569\]

12. The db utilities (db_xxxx) no longer operate on replication clients that are being automatically initialized. The DB_REP_LOCKOUT error is now returned. \[#21593\]

13. Using DB_TXN_SNAPSHOT on an HA client will now result in an error. \[#21601\]

14. Fixed a bug that prevented a sub-database from being created under the directory identified in DB-\>set_create_dir. \[#21603\]

15. Fixed a race condition on a cursor when using a multi-threaded application with the SQL API. \[#21714\]

16. Fixed a race condition in the failchk code when cleaning up mutexes. \[#21796\]

17. Fixed a resource leak in the db-\>verify() function for btrees. The bug would slow down verification and possibly cause it to run out of memory. \[#21917\]

18. Removed a potential hang when compacting databases with many duplicates. \[#21975\]

19. Fixed a bug that caused a crash when reentering dbsql and specifying replication=on for a SQL database where replication was already enabled. \[#22116\]

20. Fixed an incorrect recursive call dealing with joins. \[#22398\]

21. Fix build failures in ado.net \[#22405\]

22. Fixed a bug that could cause a JDBC build failure on recent versions of Visual Studio. \[#22497\]

23. Correct build problems when building dbstl with gcc-4.7.3 \[#22615\]

### Changes between 11.2.5.3.15 and 11.2.5.3.21

1.  Fixed incompatibility problems of Java DPL with JDK7, so DPL will now work with JDK7. \[#20586\]

2.  Added a flag to allow database locking to be disabled from the SQL API. \[#20928\]

3.  Fixed a bug that could allocate a heap data page in a region after the region creation has been undone. \[#20939\]

4.  Redundant whitespaces are now ignored in DB_CONFIG lines pertaining to directories, e.g. set_data_dir. \[#20158\]

5.  Fixed a rare race condition that could cause a crash if two processes opened the same database at the same time. \[#21041\]

6.  Fixed a bug that caused DB_ENV-\>backup to stop early if DB_BACKUP_FILES was not set and a non-DB file was in the data directory. \[#21076\]

7.  Fixed missing cross compiling capability for the JDBC driver. \[#21101\]

8.  Allow the same system/machine to host both a master and a replica database through the use of relative pathnames. \[#21105\]

9.  Fixed a bug in the Java API where EnvironmentConfig.setCreateDir would fail to configure the environment. \[#21127\]

10. Fixed an assert failure in btreeCompare when allocating memory in the wrong thread was causing a memory leak. \[#21232\]

11. Fixed a bug in the Java API where concurrent operations that change the database schema could lead to a hang. \[#21265\]

12. Added JDBC code to the code base and updated the windows build files to include the JDBC solution. \[#21294\]

13. Fixed a bug where the heap's region size was not getting swapped correctly in mixed-endian environments. \[#21295\]

14. Fixed a bug in the db_sql_jdbc project file for vs2010 that was preventing it from building correctly. \[#21332\]

### Database or Log File On-Disk Format Changes

1.  Existing database file formats were unchanged in library version 11.2.5.3.

2.  The log file format changed in library version 11.2.5.3.

### New Features

1.  Added support for verifying named in-memory dbs. \[#16941\]

2.  Added an integer key comparison function to improve performance through the SQL API. \[#19609\]

3.  Support build on the platforms where pthread_t is a struct. \[#19876\]

4.  Added an API call so the user can specify the size of the region in a heap db. \[#19914\]

5.  Improved Replication Manager's ability to recover from the (perhaps rare) phenomenon of two sites trying to connect to each other simultaneously, which used to result in loss of both connections, requiring a retry after the CONNECTION_RETRY timeout period. \[#19980\]

6.  Enhanced the interface for copying databases for a hot backup. Added configure support for --enable-atomicfileread. \[#20129\]

7.  Enhaced the log reading routine to detect that a log file is missing rather than returning that a zero length record was found. \[#20130\]

8.  Added pragma bdbsql_shared_resources to set or report the maximum amount of memory to be used by shared structures in the main environment region and bdbsql_lock_tablesize to set or report the number of buckets in the lock object hash table. These are advanced tuning features for applications with large number of tables or needs to reduce locking on concurrent long running transactions. \[#20156\]

9.  Added set_metadata_dir() and get_metadata_dir() to enable storage of persistent metadata files in a location other than the environment home directory. \[#20174\]

10. Improved the error handling through the SQL API. Errors can be sent to a file with the use of the BDBSQL_ERROR_FILE pragma. \[#20213\]

11. Database handles can now be configured to give exclusive access to the database. \[#20331\]

12. XA transactions will now use transaction snapshots if the XA databases they operate on were configured with DB_MULTIVERSION. \[#20332\]

13. Added additional stats fields into the C# API \[#20693\]

14. Added pragma bdbsql_single_process to keep the Berkeley DB environment information on the heap instead of in shared memory. This option cannot be used if the database is accessed from multiple processes. \[#20789\]

15. Improved the ability of DB-\>compact to move DB_HASH database pages to the begining of the file. \[#20815\]

### Database Environment Changes

1.  Fixed a bug that could cause a segmentation violation when closing an environment handle which has open database handles on partition databases. \[#20281\]

2.  Fixed a bug that could cause a segmentation violation if a region was extended leaving a very small fragment at the end. \[#20414\]

3.  Changed the behavior of the DB_REGISTER \| DB_RECOVER flag combination, so that recovery is always run if the environment panic bit is set. \[#20767\]

### Access Method Changes

1.  Fixed a bug were database configuration settings could be lost when the database was opened if the open operation was blocked for any amount of time. \[#20860\]

2.  Fixed a bug that bulk update operations did not work correctly on compressed databases. \[#19017\]

3.  Improved the log flushing performance when ftruncate() is not available on a system. \[#19725\]

4.  When performing partial puts in a heap database, empty pieces will no longer be left in a split record chain. \[#20052\]

5.  Fixed a bug where, on systems without FTRUNCATE, db_verify will return an error for truncated heap databases. \[#20195\]

6.  Fixed a bug where BDB could run out of mutexes when many databases are renamed. \[#20228\]

7.  Fixed a bug where the metadata page in hash databases would not be flushed to disk. \[#20265\]

8.  Fixed a bug that could leave deleted pages from a HEAP database in the buffer cache. \[#20309\]

9.  Fixed a bug where the library would fail to put records with overflow keys into hash duplicate database. \[#20329\]

10. Fixed a bug in DB-\>compact of btrees that could cause a bad pointer reference. \[#20360\]

11. Fixed a bug that could cause the last page number stored on the metadata page to be wrong after rolling forward a db-\>compact operation that freed more pages than will fit in a single log record. \[#20646\]

12. Fixed a bug that could cause DB-\>stat to block on a mutex while holding a lock on the metadata page. \[#20770\]

13. Fixed a bug that could cause DB-\>compact of a DB_HASH database to fail to mark a page it updated as dirty. \[#20778\]

14. Fixed a bug where internal HEAP structures were not rebuilt during database handle refresh. \[#20821\]

15. Fixed a bug with secondary indices, off-page duplicates and DB_READ_COMMITTED which could erroneously release the lock on the page holding a returned record. \[#20853\]

16. Fixed a bug that could cause a hang or improperly report an empty queue when the queue record numbers wrapped around at 2^32. \[#20956\]

17. Fixed a bug on Linux or Windows that could generate a checksum error if a database file was being opened while the meta data page happened to be flushed from the cache. \[#20996\]

### SQL API Changes

1.  Fixed several memory leaks in the Online Backup API. \[#19850\]

2.  Fixed a bug in the SQL API when using large blob items and multiple concurrent connections. \[#19945\]

3.  To avoid a race condition that could cause a snapshot reader to see a wrong version it is now not permitted to open a DB handle specifying DB_MULTIVERSION if that database is currently opened by a handle which did not specify DB_MULTIVERSION. \[#19940\]

4.  Pragma replication=on can now enable replication on an existing database. Turning replication off is now permanent. \[#20180\]

5.  Fixed a bug in the SQL API where it was possible for a schema update to be ignored when accessing a database from multiple processes. \[#20319\]

6.  Fixed a bug where aborting an exclusive transaction followed by an auto-commit read operation causes an assert failure. \[#20567\]

7.  Fixed a bug in the SQL API where using the journal_mode pragma could cause a crash when used as the first operation in a connection on an existing database. \[#20620\]

8.  Turn off the DBSQL encryption option on Windows/WinCE by default to match the behavior on the other platforms. \[#20671\]

9.  Renamed the BDBSQL_OMIT_SHARING preprocessor flag to BDBSQL_SINGLE_PROCESS. \[#20789\]

10. Fixed a bug dealing with handle lock modes not reflecting the correct state which was causing a deadlock in the SQL API. \[#20862\]

### Java-specific API changes

1.  Added ReplicationManagerConnectionStatus class and ReplicationManagerSiteInfo.getConnectionStatus(). Deprecated ReplicationManagerSiteInfo.isConnected(). \[#18068\]

2.  Updated EID_MASTER to be "public static final" so that it would be exposed in Java docs. \[#20184\]

3.  Fixed a bug where calls that return Stat objects could cause a segfault. \[#20377\]

### Replication Changes

1.  Fixed quorum computation when most sites are unelectable. \[#15251\]

2.  Made Replication more resilient to random input on its port. \[#15712\]

3.  Fixed a bug where the datadir structure was not maintained during an internal init. \[#19041\]

4.  Fixed a repmgr memory leak when using DB_PRIVATE. \[#19363\]

5.  Fixed a minor bug to handle ENOMEM when using an in-memory temp database. \[#20197\]

6.  Fixed a bug where multiple long running transactions across checkpoints could cause Log Sequence errors on client systems. \[#20421\]

7.  Fixed a bug where multiple Replication Manager processes would sometimes not all conform to replication-group-aware log archiving. \[#20342\]

8.  Fixed a bug where a Replication Manager master could stop functioning after accepting an obsolete group membership site list from another site. \[#21804\]

### Locking Subsystem Changes

1.  Fixed a bug that could cause an early lock timeout if a previous error left a lock timeout value set. \[#19973\]

### Logging Subsystem Changes

1.  Fixed a bug which could cause an incompletely written log record to be recognized as valid, resulting in recovery failing with the message "Illegal record type \<integer, usually 0\> in log". \[#17851\]

2.  Fixed a bug where printlog would fail on in-memory heap databases. \[#20269\]

### Memory Pool Subsystem Changes

1.  Fixed a bug which overstated the number of clean and dirty pages evicted from the cache. \[#20410\]

2.  Fixed a bug that left a small fragment at the end of a region when extending. \[#20414\]

3.  Fixed a bug where the file bucket was always zero when creating a mpoolfile using the mpool API. \[#20468\]

4.  Fixed a bug with multiversion concurrency control which could cause versions of pages to remain in the cache even though they are no longer needed. \[#20570\]

5.  The memory pool allocator will now start freezing MVCC versions of buffers if it sees more than 1/4 of the available buffers are taken up by versions. \[#20836\]

### Mutex Subsystem Changes

1.  Fixed a bug in which DB_ENV-\>mutex_set_align() could cause DB_ENV-\>mutex_stat_print(dbenv, DB_STAT_ALL) to display only the first mutex. \[#20522\]

2.  Fixed a bug with DB_ENV-\>mutex_stat_print() in which the information on some mutexes would not be displayed, if any mutex had been freed and not yet reallocated. \[#20533\]

3.  Fix a race condition between DB_ENV-\>failchk() and the allocation of a mutex. \[#21796\]

### Transaction Subsystem Changes

1.  Fixed a bug where a malloc failure could result in a segfault when doing a put on a database with secondaries. \[#20641\]

### Utility Changes

1.  Fixed a bug that would cause verify to call the wrong compare function if there are user defined compare functions used and the database has multilevel off page sorted duplicate trees. \[#20284\]

2.  Fixed a bug that could cause recovery to fail if DB-\>compact moved the meta data page of a HASH subdatabase. \[#20708\]

3.  Fixed three problems with db_hotbackup's backup of transaction logs. A hot backup did not use any configured log directory, but would try to open the logs in the environment home. The second fix corrected an error path, in which the memory was freed by the wrong function, possibly causing a guard byte error. The third fix fixed the issue that a wrong message would be displayed when only "-l" was specified. \[#21313\]

### Configuration, Documentation, Sample Apps, Portability and Build Changes

1.  The DB_CONFIG configuration commands which specify directory pathnames ("set_data_dir", "set_lg_dir", and "set_tmp_dir") now accept names containing whitespace characters. \[#20158\]

### Known Bugs

1.  If two SQL processes are concurrently altering the schema of the same tables in a database, there is a race condition that can cause the application to hang. \[#20513\]

2.  Replication groups including machines of different endianness do not support the heap access method. \[#21016\]

3.  If a txn that is attempting to remove a region page from a heap database is aborted and another txn is trying to update that same page then it can cause the original txn to abort. This is timing dependant. \[#20939\]

4.  Utilities can operate on a replication client which is being automatically initialized and may therefore be in an inconsistent state. This can cause the utility to fail or to return invalid results. You can use replication statistics to check the site's role (st_status) and master generation (st_gen) before and after a utility runs; if neither has changed, the utility's results are valid. \[#21593\]
