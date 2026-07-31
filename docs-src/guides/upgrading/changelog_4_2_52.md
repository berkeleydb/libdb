---
title: "Berkeley DB 4.2.52 Change Log"
api-name: "Berkeley DB 4.2.52 Change Log"
source: docs/upgrading/changelog_4_2_52.html
---
## Berkeley DB 4.2.52 Change Log

<span class="sect2"> [Database or Log File On-Disk Format Changes:](changelog_4_2_52.md#idp50822856) </span>

<span class="sect2"> [New Features:](changelog_4_2_52.md#idp50784344) </span>

<span class="sect2"> [Database Environment Changes:](changelog_4_2_52.md#idp50809288) </span>

<span class="sect2"> [Concurrent Data Store Changes:](changelog_4_2_52.md#idp50822104) </span>

<span class="sect2"> [General Access Method Changes:](changelog_4_2_52.md#idp50824288) </span>

<span class="sect2"> [Btree Access Method Changes:](changelog_4_2_52.md#idp50825368) </span>

<span class="sect2"> [Hash Access Method Changes:](changelog_4_2_52.md#idp50844704) </span>

<span class="sect2"> [Queue Access Method Changes:](changelog_4_2_52.md#idp50828568) </span>

<span class="sect2"> [Recno Access Method Changes:](changelog_4_2_52.md#idp50858440) </span>

<span class="sect2"> [C++-specific API Changes:](changelog_4_2_52.md#idp50832248) </span>

<span class="sect2"> [Java-specific API Changes:](changelog_4_2_52.md#idp50815840) </span>

<span class="sect2"> [Tcl-specific API Changes:](changelog_4_2_52.md#idp50867864) </span>

<span class="sect2"> [RPC-specific Client/Server Changes:](changelog_4_2_52.md#idp50852544) </span>

<span class="sect2"> [Replication Changes:](changelog_4_2_52.md#idp50858528) </span>

<span class="sect2"> [XA Resource Manager Changes:](changelog_4_2_52.md#idp50877816) </span>

<span class="sect2"> [Locking Subsystem Changes:](changelog_4_2_52.md#idp50865088) </span>

<span class="sect2"> [Logging Subsystem Changes:](changelog_4_2_52.md#idp50868008) </span>

<span class="sect2"> [Memory Pool Subsystem Changes:](changelog_4_2_52.md#idp50865504) </span>

<span class="sect2"> [Transaction Subsystem Changes:](changelog_4_2_52.md#idp50845064) </span>

<span class="sect2"> [Utility Changes:](changelog_4_2_52.md#idp50858944) </span>

<span class="sect2"> [Configuration, Documentation, Portability and Build Changes:](changelog_4_2_52.md#idp50892568) </span>

### Database or Log File On-Disk Format Changes:

1.  Queue databases that use encryption or data checksum features with extent files will need to be dumped and reloaded prior to using with release 4.2. For more details, see <a href="upgrade_4_2_queue.md" class="xref" title="Queue access method">Queue access method</a>. \[#8671\]
2.  The on-disk log format changed.

### New Features:

1.  Add support for a reduced memory footprint build of the Berkeley DB library. \[#1967\]
2.  Add the DB_MPOOLFILE-\>set_flags interface which disallows the creation of backing filesystem files for in-memory databases. \[#4224\]
3.  Add cache interfaces to limit the number of buffers written sequentially to allow applications to bound the time they will monopolize the disk. \[#4935\]
4.  Support auto-deletion of log files. \[#0040\] \[#6252\]
5.  The new Java DBX API for Berkeley DB allows Java programmers to use a familiar Java Collections style API, including Map, while interacting with the transactional Berkeley DB core engine. \[#6260\]
6.  Support auto-commit with the DB-\>get method's consume operations. \[#6954\]
7.  Add "get" methods to retrieve most settings. \[#7061\]
8.  Add Javadoc documentation to the Berkeley DB release. \[#7110\]
9.  Add support to Concurrent Data Store to allow duplication of write cursors. \[#7167\]
10. Add C++ utility classes for iterating over multiple key and data items returned from a cursor when using the DB_MULTIPLE or DB_MULTIPLE_KEY flags. \[#7351\]
11. Add CamelCased methods to the Java API. \[#7396\]
12. Add the DB_MPOOLFILE-\>set_maxsize interface to enforce a maximum database size. \[#7582\]
13. Add a toString() method for all Java \*Stat classes (DbBtreeStat, DbHashStat, DbMpoolStat, etc.). This method creates a listing of values of all of the class member variables. \[#7712\]

### Database Environment Changes:

1.  Add cache interfaces to limit the number of buffers written sequentially to allow applications to bound the time they will monopolize the disk. \[#4935\]
2.  Fix a bug which could cause database environment open to hang, in database environments supporting cryptography. \[#6621\]
3.  Fix a bug where a database environment panic might result from an out-of-disk-space error while rolling back a page allocation. \[#6694\]
4.  Fix a bug where a database page write failure, in a database environment configured for encryption or byte-swapping, could cause page corruption. \[#6791\]
5.  Fix a bug where DB-\>truncate could drop core if there were active cursors in the database. \[#6846\]
6.  Fix a bug where for databases sharing a physical file required a file descriptor per database. \[#6981\]
7.  Fix a bug where the panic callback routine was only being called in the first thread of control to detect the error when returning DB_RUNRECOVERY. \[#7019\]
8.  Fix a bug where a transaction which contained a remove of a subdatabase and an allocation to another subdatabase in the same file might not properly be aborted. \[#7356\]
9.  Fix a bug to now disallow DB_TRUNCATE on opens in locking environments, since we cannot prevent race conditions. In the absence of locking and transactions, DB_TRUNCATE will truncate ANY file for which the user has appropriate permissions. \[#7345\]
10. Fix several bugs around concurrent creation of databases. \[#7363\]
11. Change methods in DbEnv that provide access to statistics information so that they now return instances of the proper classes. \[#7395\]
12. Replace the DB-\>set_cache_priority API with the DB_MPOOLFILE-\>set_priority API. \[#7545\]
13. Fix a bug where a failure during a creation of a subdatabase could then fail in the dbremove cleanup, causing a crash. \[#7579\]
14. Allow creating into a file that was renamed within the same transaction. \[#7581\]
15. Fix a bug where DB_ENV-\>txn_stat could drop core if there are more-than-expected active transactions. \[#7638\]
16. Change Berkeley DB to ignore user-specified byte orders when creating a database in an already existing physical file. \[#7640\]
17. Fix a bug where a database rename that is aborted would leak some memory. \[#7789\]
18. Fix a bug where files could not be renamed or removed if they were not writable. \[#7819\]
19. Fix a bug where an error during a database open may leak memory in the mpool region. \[#7834\]
20. Fix a bug where the DB_ENV-\>trickle_sync method could flush all of the dirty buffers in the cache rather than a subset. \[#7863\]
21. Fix a bug where an attempt to rename or remove an open file in the same transaction could succeed, even though this is not allowed and will not work on Windows. \[#7917\]
22. Fix a bug where if a recovery interval in the log contained only database opens then a recovery might report "Improper file close". \[#7886\]
23. Add a flag, DB_INIT_REP to DB_ENV-\>open to initialize Replication subsystem. \[#8299\]
24. Fix a bug where file remove and rename operations would not block each other if they were in different transactions. \[#8340\]
25. Change Berkeley DB to not propagate error returns from the application's rep_send function out of the Berkeley DB API. \[#8496\] \[#8522\]
26. Remove restriction that DB_TRUNCATE is not allowed on files with subdatabases. This restriction was introduced in 4.1.25. \[#8852\]

### Concurrent Data Store Changes:

1.  Fix a bug where opens with other threads/processes actively acquiring locks on database handles could deadlock. \[#6286\]
2.  Add support to Concurrent Data Store to allow duplication of write cursors. \[#7167\]

### General Access Method Changes:

1.  Fix a bug where the truncate of a database with associated secondary databases did not truncate the secondaries. \[#6585\]
2.  Fix a bug in which an out-of-disk condition during a transactional database create, remove, or rename could cause a crash. \[#6695\]
3.  Fix a bug where system errors unknown to the C library could cause Berkeley DB utilities to drop core on Solaris. \[#6728\]
4.  Fix a bug where Berkeley DB could overwrite incorrectly formatted files rather than returning an error to the application during open. \[#6769\]
5.  Fix a bug DB handle reference counts were incorrect, leading to spurious warning about open DB handles. \[#6818\]
6.  Fix a bug where cursor adjustments across multiple DB handles could fail. \[#6820\]
7.  Fix a bug where a failure during open could result in a hang. \[#6902\]
8.  Fix a bug where repeated failures during certain stages of opens could cause error messages to appear during recovery. \[#7008\]
9.  Fix a bug in secondary indices with multiple threads calling DBC-\>put that resulted in DB_NOTFOUND being returned. \[#7124\]
10. Fix a bug where database verification might reference memory which was previously freed after reporting an error. \[#7137\]
11. Rename the DB_CHKSUM_SHA1 to DB_CHKSUM as Berkeley DB only uses SHA1 for encrypted pages, not for clear text pages. \[#7095\]
12. Fix a bug where DB-\>rename could fail silently if the underlying system rename call failed. \[#7322\]
13. Fix a bug where Berkeley DB failed to open a file with FCNTL locking and 0-length files. \[#7345\]
14. Prohibit the use of the DB_RMW flag on get operations for DB handles opened in transactional mode. \[#7407\]
15. Standardize when Berkeley DB will return DB_LOCK_NOTGRANTED, or throw DbLockNotGrantedException, versus returning DB_LOCK_DEADLOCK or throwing DbDeadlockException. Fix bugs in the C++ and Java APIs where DbException was thrown, encapsulating DB_LOCK_NOTGRANTED, rather than throwing DbLockNotGrantedException. \[#7549\]
16. Fix a bug where Berkeley DB could hang on a race condition if a checkpoint was running at the same time another thread was closing a database for the last time. \[#7604\]
17. Fix several bugs that made multiple filesystem level operations inside a single transaction break. \[#7728\]
18. Fix a memory leak in the abort path of a sub-database create. \[#7790\]
19. Fix a race condition with file close that could cause NULL pointer deference under load. \[#8235\]
20. Fix a bug to correct the calculation of the amount of space needed to return off page duplicates using the DB_MULTIPLE interface. \[#8437\]
21. Fix a bug where the duplicate data item count could be incorrect if a cursor was used to first overwrite and then delete a duplicate which was part of a set of duplicates large enough to have been stored outside the standard access method pages. \[#8445\]
22. Fix a bug where The DB_MULTIPLE interface might fail to return the proper duplicates in some edge cases. \[#8485\]
23. Fix a bug where DB-\>get(...DB_MULTIPLE) would not return a reasonable estimate of the buffer size required to return a set of duplicates. \[#8513\]
24. Fix a bug where the DbCursor.count method could return the wrong count in the case of small (on-page) duplicate sets, where a still-open cursor has been used to delete one of the duplicate data items. \[#8851\]
25. Fix a bug where a non-transactional cursor using DB_MULTIPLE_KEY could briefly be left pointing at an unlocked page. This could lead to a race condition with another thread deleting records resulting in the wrong record being deleted. \[#8926\]
26. Fix a bug where a key/data item could be lost if a cursor is used to do a delete, and then immediately used to do an insert which causes a set of duplicates to be shifted to an off-page Btree. \[#9085\]

### Btree Access Method Changes:

1.  Fix a bug where a deleted item could be left on a database page causing database verification to fail. \[#6059\]
2.  Fix a bug where a page may be left pinned in the cache if a deadlock occurs during a DB-\>put operation. \[#6875\]
3.  Fix a bug where a deleted record may not be removed from a Btree page if the page is split while another cursor is trying to delete a record on the page. \[#6059\]
4.  Fix a bug where records marked for deletion were incorrectly counted when retrieving in a Btree by record number. \[#7133\]
5.  Fix a bug where a page and lock were left pinned if an application requested a record number past the end of the file when retrieving in a Btree by record number. \[#7133\]
6.  Fix a bug where deleted keys were included in the key count for the DB-\>stat call. \[#7133\]
7.  Fix a bug where specifying MULTIPLE_KEY and NEXT_DUP to the bulk get interfaces might return the wrong data if all the duplicates could not fit in a single buffer. \[#7192\]
8.  Remove assertions that triggered failures that were correct executions. \[#8032\]
9.  Fix a bug where duplicate data items were moved onto overflow pages before it was necessary. \[#8082\]
10. Fix a bug where the DB-\>verify method might incorrectly complain about a tree's overflow page reference count. \[#8061\]
11. Fix a bug that could cause DB_MULTIPLE on a Btree database to return an incorrect data field at the end of buffer. \[#8442\]
12. Fix a bug where DBC-\>c_count was returning an incorrect count if the cursor was positioned on an item that had just been deleted. \[#8851\]
13. Remove the test for bt_maxkey in the Btree put code. If it is set to 1 it can cause an infinite loop. \[#8904\]

### Hash Access Method Changes:

1.  Fix a bug where Hash databases could be corrupted on filesystems that do not zero-fill implicitly created blocks. \[#6588\]
2.  Fix a bug where creating a Hash database with an initial size larger than 4GB would fail. \[#6805\]
3.  Fix a bug where a page in an unused hash bucket might not be empty if there was a disk error while writing the log record for the bucket split. \[#7035\]
4.  Fix a bug where two threads opening a hash database at the same time might deadlock. \[#7159\]
5.  Fix a bug where a hash cursor was not updated properly when doing a put with DB_NODUPDATA specified. \[#7361\]
6.  Fix a bug that could cause DB_MULTIPLE_KEY on Hash databases to return improper results when moving from a key with duplicates to a key without duplicates. \[#8442\]

### Queue Access Method Changes:

1.  Fix a bug where opening an in-memory Queue database with extent size specified will dump core. \[#6795\]
2.  Support auto-commit with the DB-\>get method's consume operations. \[#6954\]
3.  Fix a bug where calling the sync method on a queue database with extents may hang if there are active consumers. \[#7022\]
4.  Fix a bug where a get(...MULTIPLE...) might lead to an infinite loop or return the wrong record number(s) if there was a deleted record at the beginning of a page or the buffer was filled exactly at the end of a page. \[#7064\]
5.  Fix a bug where a database environment checkpoint might hang if a thread was blocked waiting for a record while doing a DB_CONSUME_WAIT on a Queue database. \[#7086\]
6.  Fix a bug where queue extent files would not be removed if a queue with extents was removed and its record numbers wrapped around the maximum record number. \[#7191\]
7.  Fix a bug where a DB-\>remove of an extent based Queue with a small number of pages per extent would generate a segmentation fault. \[#7249\]
8.  Fix a bug where verify and salvage on queues with extent files did not consider the extent files. \[#7294\]
9.  Fix a bug when transaction timeouts are set in the environment they would get applied to some non-transactional operations and could cause a failure during the abort of a queue operation. \[#7641\]
10. Fix a bug when the record numbers in a queue database wrap around at 232, a cursor positioned on a record near the head of the queue that is then deleted, may return DB_NOTFOUND when get is specified with DB_NEXT rather than the next non-deleted record. \[#7979\]
11. Fix a bug where a record lock will not be removed when the first record in the queue is deleted without a transaction (not using DB_CONSUME). \[#8434\]
12. Fix a bug where byte swapping was not handled properly in queue extent files. \[#8358\]
13. Fix a bug where Queue extent file pages were not properly typed, causing the extent files not to use encryption or checksums, even if those options had been specified. This fix requires a database upgrade for any affected Queue databases. \[#8671\]
14. Fix a bug where truncating a queue with extents may fail to remove the last extent file. \[#8716\]
15. Fix a bug where a rename or remove of a QUEUE database with extents might leave empty extent files behind. \[#8729\]
16. Fix a bug where on Windows operating systems a "Permission denied" error may be raised if a Queue extent is reopened while it is in the process of being unlinked. \[#8710\]

### Recno Access Method Changes:

1.  Fix a bug where the DB-\>truncate method may return the wrong record count if there are deleted records in the database. \[#6788\]
2.  Fix a bug where internal nodes of Recno trees could get wrong record count if a log write failed and the log was later applied during recovery. \[#6841\]
3.  Fix a bug where a cursor next operation could infinitely loop after deleting a record, when the deleted record was immediately followed by implicitly created records. \[#8133\]

### C++-specific API Changes:

1.  Document the DB-\>del method can return DB_KEYEMPTY for Queue or Recno databases. The C++ and Java APIs now return this value rather than throwing an exception. \[#7030\]
2.  Add "get" methods to retrieve most settings. \[#7061\]
3.  Fix a bug where applications calling DB-\>verify from the C++ or Java APIs could drop core. Change the DB-\>verify method API to act as a DB handle destructor. \[#7418\]
4.  Add utility classes for iterating over multiple key and data items returned from a cursor when using the DB_MULTIPLE or DB_MULTIPLE_KEY flags. These classes, DbMultipleDataIterator, DbMultipleKeyDataIterator, and DbMultipleRecnoDataIterator, mirror the DB Java API and are provided as replacements for the C macros, DB_MULTIPLE_INIT, DB_MULTIPLE_NEXT, DB_MULTIPLE_KEY_NEXT, and DB_MULTIPLE. \[#7351\]
5.  Fix a bug DbException was thrown, encapsulating DB_LOCK_NOTGRANTED, rather than throwing DbLockNotGrantedException. \[#7549\]
6.  Add the DbEnv handle to exceptions thrown by the C++ and Java APIs, where possible. \[#7303\]
7.  Fix a bug in the C++ DbEnv::set_rep_transport signature so that the envid parameter is signed. \[#8303\]
8.  Make the fields of DB_LSN public in the DbLsn class. \[#8422\]

### Java-specific API Changes:

1.  Db.put(), Dbc.get() and Dbc.put() preserve key size
2.  Dbc.get() returns DB_KEYEMPTY rather than throwing an exception
3.  The return type of Db.close() is now void. \[#7002\]

<!-- -->

1.  New Java API (com.sleepycat.dbx.\*) for the transactional storage of data using the Java Collections design pattern. \[#6569\]
2.  Fix a bug in the Java Dbt.get_recno_key_data() method when used inside callbacks. \[#6668\]
3.  Fix Java DbMpoolStat class to match the DB_MPOOL_STAT struct. \[#6821\]
4.  Fix a bug where Dbc.put expected key data even if the key was unused. \[#6932\]
5.  Fix a bug in the Java API secondary_key_create callback where memory was freed incorrectly, causing JVM crashes. \[#6970\]
6.  Re-implement the Java API to improve performance and maintenance. Fix several inconsistencies in the Java API:
7.  Document the DB-\>del method can return DB_KEYEMPTY for Queue or Recno databases. The C++ and Java APIs now return this value rather than throwing an exception. \[#7030\]
8.  Add "get" methods to retrieve most settings. \[#7061\]
9.  Add Javadoc documentation to the Berkeley DB release. \[#7110\]
10. Fix a bug that caused potential memory corruption when using the Java API and specifying the DB_DBT_REALLOC flag. \[#7215\]
11. Add the DbEnv handle to exceptions thrown by the C++ and Java APIs, where possible. \[#7303\]
12. Map existing c-style API to a more Java camel case API with Java style naming. Retained deprecated older API for the 4.2 release for backwards support in all cases except callback interfaces. Also overloaded methods such as get/pget() into multiple different get() calls to clean up call structure. \[#7378\]
13. Add CamelCased methods to the Java API. \[#7396\]
14. Fix a bug where applications calling DB-\>verify from the C++ or Java APIs could drop core. Change the DB-\>verify method API to act as a DB handle destructor. \[#7418\]
15. Fix a bug DbException was thrown, encapsulating DB_LOCK_NOTGRANTED, rather than throwing DbLockNotGrantedException. \[#7549\]
16. Add a toString() method for all Java \*Stat classes (DbBtreeStat, DbHashStat, DbMpoolStat, etc.). This method creates a listing of values of all of the class member variables. \[#7712\]
17. Remove Db.fd() method from Java API as it has no value to a Java programmer. \[#7716\]
18. Add an accessible timeout field in the DbLockRequest class, needed for the DB_LOCK_GET_TIMEOUT operation of DbEnv.lockVector. \[#8043\]
19. Fix replication method calls from Java API. \[#8467\]
20. Fix a bug where exception returns were inconsistent. \[#8622\]
21. Change the Java API so that it throws an IllegalArgumentException rather than a DbException with the platform-specific EINVAL. \[#8978\]

### Tcl-specific API Changes:

1.  Add "get" methods to retrieve most settings. \[#7061\]
2.  Brought Tcl's \$env set_flags command up to date with available flags. \[#7385\]
3.  Update Berkeley DB to compile cleanly against the Tcl/Tk 8.4 release. \[#7612\]
4.  Made txn_checkpoint publicly available. \[#8594\]

### RPC-specific Client/Server Changes:

1.  Fix two bugs in the RPC server where incorrect handling of illegal environment home directories caused server crashes. \[#7075\]
2.  Fix a bug where the DB_ENV-\>close method would fail in RPC clients if the DB_ENV-\>open method was never called. \[#8200\]

### Replication Changes:

1.  Write prepare records synchronously on replication clients so that prepare operations are always honored in the case of failure. \[#6416\]
2.  Change replication elections so that the client with the biggest LSN wins, and priority is a secondary factor. \[#6568\]
3.  Fix a bug where replicas could not remove log files because the checkpoint lsn was not being updated properly. \[#6620\]
4.  Force prepare records out to disk regardless of the setting of the DB_TXN_NOSYNC flag. \[#6614\]
5.  Add a new flag, DB_REP_NOBUFFER, which gets passed to the rep_send function specified in DBENV-\>rep_set_transport, to indicate that the message should not be buffered on the master, but should be immediately transmitted to the client(s). \[#6680\]
6.  Fix a replication election bug where Berkeley DB could fail to elect a master even if a master already existed. \[#6702\]
7.  Allow environment wide setting of DB_AUTO_COMMIT on replication clients. \[#6732\]
8.  Fix a replication bug where a client coming up in the midst of an election might not participate in the election. \[#6826\]
9.  Add log_flushes when sites become replication masters. If log_flush fails, panic the environment since the clients already have the commits. \[#6873\]
10. Fix a replication bug where a brand new client syncing up could generate an error on the master. \[#6927\]
11. Fix a bug where clients synchronize with the master when they come up with the same master after a client-side disconnect or failures. \[#6986\]
12. Fix several bugs in replication elections turned up by test rep005. \[#6990\]
13. Fix a bug where aborted hash group allocations were not properly applied on replicas. \[#7039\]
14. Fix race conditions between running client recovery and other threads calling replication and other Berkeley DB functions. \[#7402\] \[#8035\]
15. Use shared memory region for all replication flags. \[#7573\]
16. Fix a bug where log archive on clients could prematurely remove log files. \[#7659\]
17. Return an error if a non-replication dbenv handle attempts to write log records to a replication environment. \[#7752\]
18. Fix a race condition when clients applied log records, where we would store a log record locally and then never notice we have it, and need to re-request it from the master, causing the client to get far behind the master. \[#7765\]
19. Fix inconsistencies between the documentation and actual code regarding when replication methods can be called. \[#7775\]
20. Fix a bug where Berkeley DB would wait forever if a NEWMASTER message got dropped. \[#7897\]
21. Fix a bug where the master environment ID did not get set when you called DBENV-\>rep_start as a master. \[#7899\]
22. Fix a bug where operations on a queue database will not get replicated if the transactions that include the operations are committed out of order with the operations. \[#7904\]
23. Fix bugs in log_c_get where an invalid LSN could access invalid addresses. Fix bug in elections where a client upgrading to master didn't write a txn_recycle record. \[#7964\]
24. Fix a bug where REP_VERIFY_FAIL during client recovery wasn't being handled. \[#8040\]
25. Return an error if the application calls rep_process_message before calling rep_start when starting. \[#8057\]
26. Fix a bug to ensure that replication generation numbers always increase and are never reset to 1. \[#8136\]
27. Modify log message retransmission protocol to efficiently handle the case where a large number of contiguous messages were dropped at once. \[#8182\] \[#8169\] \[#8188\]
28. Fix a bug where using the wrong mutex in replication which under certain conditions could cause replication to hang. Also fix a bug where incorrectly setting the checkpoint LSN could cause recovery to take a very long time. \[#8183\]
29. Fix bug where a message could get sent to an invalid master. \[#8184\]
30. Fix a bug where a local variable in log_archive was not initialized. \[#8230\]
31. Fix a bug where elections could hang. \[#8254\]
32. Fix a bug to ensure that we can always remove/re-create the temporary replication database after a failure. \[#8266\]
33. Add a flag, DB_INIT_REP to DB_ENV-\>open to initialize Replication subsystem. \[#8299\]
34. Add new ret_lsnp argument to rep_process_message so that LSNs can be returned to clients on permanent records. Add new lsnp arg to the send callback function so that the master can know the LSNs of records as well. \[#8308\]
35. Narrow the window where we block due to client recovery. \[#8316\]
36. Fix a bug in log_c_incursor where we would not detect that a record was already in the buffer. \[#8330\]
37. Fix a bug that would allow elections to be managed incorrectly. \[#8360\]
38. Fix a bug where replicas were not maintaining meta-\>last_pgno correctly. \[#8378\]
39. Fix a bug in truncating log after recovery to a timestamp or replication-based recovery. \[#8387\]
40. Fix a bug where a checkpoint record written as the first record in a log could cause recovery to fail. \[#8391\]
41. Fix a bug where a client would return DB_NOTFOUND instead of DB_REP_OUTDATED when it was unable to synchronize with the master because it ran out of log records. \[#8399\]
42. Fix a bug where log file changes were not handled properly in replication. \[#8400\] \[#8420\]
43. Fix a bug where checking for invalid log header data could fail incorrectly. \[#8460\]
44. Fix a bug where DB_REP_PERMANENT was not being set when log records were re-transmitted. \[#8473\]
45. Modify elections so that all participants elect in the same election generation. \[#8590\]
46. Fix bug where rep_apply was masking an error return. Also return DB_RUNRECOVERY if the replication client cannot commit or checkpoint. \[#8636\]
47. Fix a bug to update the last_pgno on the meta page on free as well as alloc. \[#8637\]
48. Fix a bug to roll back the LSN on a queue database metapage if we're going to truncate the log. Fix a bug in MASTER_CHECK so we don't apply log messages from an unknown master. Fix a bug to perform a sync on rep_close. \[#8601\]
49. Fix a bug so that we reset the LSN when putting pages on the free list. \[#8685\]
50. Fix a bug where replication was not properly calling db_shalloc. \[#8811\]
51. Fix a bug where replication flags were getting set in multiple steps which could cause an Assertion Failure in log_compare. \[#8889\]
52. Fix a bug where open database handles could cause problems on clients. \[#8936\]
53. Fix a bug where in dbreg code where an fnp with an invalid fileid could be found on the lp-\>fq list. \[#8963\]
54. Fix a bug where a reader on a replication client could see partial updates when replicating databases with off page duplicates or overflow records. \[#9041\]
55. Fix a bug that could result in a self deadlock in dbreg under replication. \[#9138\]
56. Fix a memory leak in replication. \[#9255\]

### XA Resource Manager Changes:

1.  Fix a bug where a failed write during XA transaction prepare could result in a checksum error in the log. \[#6760\]
2.  Fix a bug where we were not properly handling DB_AUTO_COMMIT in XA transactions and where we were not honoring the XA transaction during an XA-protected open. \[#6851\]
3.  Add infrastructure support for multithreaded XA. \[#6865\]
4.  Display XA status and ID as part of db_stat -t output. \[#6413\]

### Locking Subsystem Changes:

1.  failure to remove dirty read locks prior to aborting a transaction,
2.  calling upgrade on other than WWRITE locks,
3.  failure to remove expired locks from the locker queue,
4.  clearing the lock timeout before looking at it. \[#7267\]

<!-- -->

1.  Fix a bug where locks were not cleared in an off-page duplicate cursor. \[#6950\]
2.  Fix a bug where a deadlock may not be detected if dirty reads are enabled and the deadlock involves an aborting transaction. \[#7143\]
3.  Fix a bug where a transaction doing updates while using dirty read locking might fail while aborting the transaction with a deadlock. Several other locking issues were also fixed:
4.  Fix a bug when dirty reads are enabled a writer might be blocked on a lock that it had previously obtained. Dirty readers would also wait behind regular readers when they could have safely read a page. \[#7502\]
5.  Fix a bug where a DB-\>put using CDB gets a lock timeout then the error "Closing already closed cursor". \[#7597\]
6.  Modify the maximum test-and-set mutex sleep for logical page locks at 10ms, everything else at 25ms. \[#7675\]
7.  Fix a bug where the DB_LOCK_TIMEOUT mode of env-\>lock_vec could hang. \[#7682\]
8.  Fix a bug where running with only transaction timeouts for deadlock detection might deadlock without being detected if more than one transaction times out while trying to avoid searching a Btree on repeated inserts. \[#7787\]
9.  Fix a bug that could cause detection to not run when there was a lock that should be timed out. \[#8588\]
10. Fix a bug with using dirty reads with subtransactions. If a writing subtransaction aborts and then is blocked, the deadlock may not be detected. \[#9193\]
11. Fix a bug where handle locks were not being correctly updated when releasing read locks during transaction prepare. \[#9275\]

### Logging Subsystem Changes:

1.  Fix a bug where if a write error occurred while committing a transaction with DB_WRITE_NOSYNC enabled the transaction may appear to be committed in the log while it was really aborted. \[#7034\]
2.  Fix a bug where multiprocess applications could violate write-ahead logging requirements if one process wrote a log record but didn't flush it, the current log file then changed, and another process wrote a database page before the log record was written to disk. \[#6999\]
3.  Fix a bug where fatal recovery could fail with a "Transaction already committed" error if recovery had been run and there are no active transactions in the part of the log following the last checkpoint. \[#7234\]
4.  Fix a bug where recovery would fail to put freed pages onto the free list, when both committed and aborted subtransactions that allocated new pages were present. This only affected prepared transactions. \[#7403\]
5.  Fix a bug where open errors during recovery get propagated unless they are reporting missing files, which might correctly have been removed. \[#7578\]
6.  Fix a bug so that we now validate a log file before writing to it. \[#7580\]
7.  Fix a bug where Berkeley DB could display the unnecessary error message "DB_LOGC-\>get: short read" during recovery. \[#7700\]
8.  Fix a bug where recovery may fail if it tries to reallocate a page to a file that is out of space. \[#7780\]
9.  Change Berkeley DB so that operations on databases opened in a non-transactional mode do not write records into the database logs. \[#7843\]
10. Fix a bug where Berkeley DB could timeout waiting for locks (on Queue databases) during recovery. \[#7927\]
11. Fix a bug in truncating log after recovery to a timestamp or replication-based recovery. \[#8387\]
12. Fix a bug where recovery can be slow if the log contains many opens of files which contain multiple databases. \[#8423\]
13. Fix a bug where a file id could be used before its open was logged. \[#8496\]
14. Fix a bug where recovery would partially undo a database create if the transaction which created it spanned log files and not all of the log files were present during recovery. \[#9039\]

### Memory Pool Subsystem Changes:

1.  Fix a bug where checksummed files could not be read on different endian systems. \[#6429\]
2.  Fix a bug where read-only databases were not mapped into memory but were instead read through the Berkeley DB buffer cache. \[#6671\]
3.  Fix a bug where Berkeley DB could loop infinitely if the cache was sized so small that all of its pages were simultaneously pinned by the application. \[#6681\]
4.  Fix a bug where DbEnv.sync could fail to write a page if another thread unpinned the page at the same time and there were no other pages in that hash bucket. \[#6793\]
5.  Fix a bug where threads of control may hang if multiple threads of control are opening and closing a database at the same time. \[#6953\]
6.  Fix a bug where a database created without checksums but later opened with checksums would result in a checksum error. \[#6959\]
7.  Fix a bug where a multiprocess application suite could see incorrect data if one process opened a non-checksummed database
8.  Change to avoid database open and flush when handles are discarded, if the handle was never used to write anything. \[#7232\]
9.  Fix a bug where applications dirtying the entire cache in a single database operation would see large performance degradation. \[#7273\]
10. Fix a bug where contention in the buffer pool could cause the buffer allocation algorithm to unnecessarily sleep waiting for buffers to be freed. \[#7572\]

### Transaction Subsystem Changes:

1.  Fix a bug where disk write errors in encrypted database environments, causing transaction abort, could corrupt the log. \[#6768\]
2.  Fix a bug where catastrophic recovery may fail on a log which has a prepared transaction which aborted the allocation of a new page and was rolled forward previously by another recovery session. \[#6790\]
3.  Fix a bug where a transaction that contains a database truncate followed by page allocations, may not properly undo the truncate if aborted. \[#6862\]
4.  Fix a bug which causes Berkeley DB to checkpoint quiescent database environments. \[#6933\]
5.  Fix a bug where if a transaction prepare fails while writing the prepare log record, and it contains a subtransaction which did an allocation later, recovery of the database may fail with a log sequence error. \[#6874\]
6.  Do not abort prepared but not yet completed transactions when closing an environment. \[#6993\]
7.  Fix a bug where operations on the source of a rename in the same transaction would fail. \[#7537\]
8.  Fix a bug where a parent transaction which aborts when it tries to write its commit record could fail with a log sequence error, if the parent transaction has an aborted child transaction which allocated a new page from the operating system. \[#7251\]
9.  Fix a bug where Berkeley DB could try to abort a partial transaction because it contained a partial subtransaction. \[#7922\]
10. Fix a bug where Berkeley DB could drop core when transactions were configured without locking support. \[#9255\]

### Utility Changes:

1.  Fix a bug where db_load could core dump or corrupt record numbers by walking off the end of a string. \[#6985\]
2.  Fix a bug where db_load could run out of locks when loading large numbers of records. \[#7173\]
3.  Fix a bug where db_dump could drop core when salvaging unaligned entries on a Btree page. \[#7247\]
4.  Fix a bug where hash statistics did not include overflow items in the count of database data elements. \[#7473\]
5.  Fix a bug where an corruption in an overflow page list could cause DB-\>verify to infinitely loop. \[#7663\]
6.  Fix a bug where verify could display extraneous error messages when verifying a Btree with corrupt or missing pages. \[#7750\]
7.  Fix a bug that could cause the db_stat utility to display values larger than 100 for various percentages. \[#7779\]
8.  Fix a memory overflow bug in db_load. \[#8124\]
9.  Fix a minor leak when verifying queue databases. \[#8620\]

### Configuration, Documentation, Portability and Build Changes:

1.  Add support for a reduced memory footprint build of the Berkeley DB library. \[#1967\]
2.  Change DB_SYSTEM_MEM on Windows to fail immediately when opening an environment whose regions were deleted on last close. \[#4882\]
3.  Update queue.h to current FreeBSD version. \[#5494\]
4.  Support for and certification under Tornado 2.2/VxWorks 5.5. \[#5522\]
5.  Add support for IBM OS/390 using the IBM C compiler. \[#6486\]
6.  Specify -pthread as a compile flag for Tru64 systems, not just as a linker flag. \[#6637\]
7.  Remove automatic aggregate initialization for non-ANSI compilers. \[#6664\]
8.  Fix a link error ("GetLongPathNameA could not be located in the dynamic link library KERNEL32.dll") that prevented Berkeley DB from loading on Windows NT. \[#6665\]
9.  Remove use of U suffix in crypto build to denote unsigned integers for non-ANSI compilers. \[#6663\]
10. Fix Java API documentation problems where API return values were int and should have been void, or vice versa. \[#6675\]
11. Add an include of \<sys/fcntl.h\> for old Solaris systems with the directio call. \[#6707\]
12. Fix Java API documentation problem where the Db.associate call was missing a DbTxn handle. \[#6714\]
13. Clean up source based on gcc's -Wmissing-prototypes option. \[#6759\]
14. Ignore pread/pwrite interfaces on NCR's System V R 4.3 system. \[#6766\]
15. Fix an interface compatibility with Sendmail and Postfix releases. \[#6769\]
16. Fix warnings when the Tcl API was built without TEST_CONFIG defined. \[#6789\]
17. Change Win32 mutexes to use the shared code for all mutexes to fix handle leak. \[#6822\] \[#6853\]
18. Fix the Windows/Tcl API export list for Berkeley DB XML. \[#6931\]
19. Add the --enable-mingw configuration option to build Berkeley DB for MinGW. \[#6973\]
20. Add a CPU pause to the mutex spinlock code to improve performance on newer Pentium CPUs. \[#6975\]
21. Upgrade read-only file descriptors to read-write during checkpoint, it's an error to call FlushFileBuffers on a read-only Windows file handle. \[#7051\]
22. Fix configure so that Java applications on HP/UX can access RPC environments. \[#7066\]
23. Update Berkeley DB to use libtool 1.5 to allow building of shared libraries on various platforms. This should not be visible except for changes to the Makefile and internal build procedures. \[#7080\]
24. Fix a bug where the configure script displayed incorrect default installation directory information. \[#7081\]
25. Fix a signed/unsigned warning with some Windows compilers. \[#7100\]
26. Fix macro redefinition conflicts between queue.h and Vc7\PlatformSDK\Include\WinNT.h when building with Visual Studio.NET 7.0. \[#7103\]
27. Add a loop to retry system calls that return EBUSY. Also limit retries on EINTR to 100 times. \[#7118\]
28. Fix a bug in our use of GetDiskFreeSpace that caused access violations on some versions of Windows with DB_DIRECT_DB. \[#7122\]
29. Fix a bug where regions in system memory on Windows were incorrectly reinitialized because the magic number was overwritten. \[#7127\]
30. Change version provided to Tcl's package system to reflect Berkeley DB's major and minor number. \[#7174\]
31. Support for the Berkeley DB Embedix port has been removed. \[#7209\]
32. Merge all public C++ headers into db_cxx.h, which fixes name clashes between Berkeley DB headers and system headers (specifically mutex.h). \[#7221\]
33. Fix a bug where the configured Makefile could try and build objects for which there were no existing rules. \[#7227\]
34. Port the ex_repquote example to Windows. \[#7328\]
35. Fix a race in the ARM/gcc mutex code which could cause almost anything bad you can imagine. \[#7468\]
36. Fix a bug where shared region removal could hang. \[#7613\]
37. Fix a bug so that when using Java in Debug mode on Windows, automatically pick the Debug DLL. \[#7722\]
38. Fix configure --disable-shared so that it now creates a Makefile that installs static libraries that look the same as a regular shared build. This flag will create a libdb\<major\>.\<minor\>.a and make a libdb.a that is a symlink to it. \[#7755\]
39. Add support for OS/390 2.10 and all versions of z/OS. \[#7972\]
40. Support Java builds on Windows with spaces in the project path. \[#8141\]
41. Fix a bug where Berkeley DB mutex locking code for OS X was not multiprocessor safe. \[#8255\]
42. Add an error to DB_ENV-\>set_flags if the OS does not support Direct
43. Enable verbose error logging from the test suite on Windows. \[#8634\]
44. Fix a bug with DLL linking on Cygwin under Windows. \[#8628\]
45. Add support for JDK on HP/UX. \[#8813\]
46. Fix a bug where pathnames longer than 2KB could cause processes to core dump. \[#8886\]
47. Fix a bug in VxWorks when yielding the CPU, so that we delay at least one tick. \[#9061\]
