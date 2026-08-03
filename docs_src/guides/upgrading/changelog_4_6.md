---
title: "Berkeley DB 4.6.21 Change Log"
api-name: "Berkeley DB 4.6.21 Change Log"
source: docs/upgrading/changelog_4_6.html
---
## Berkeley DB 4.6.21 Change Log

<span class="sect2"> [4.6.21 Patches:](changelog_4_6.md#idp50449856) </span>

<span class="sect2"> [4.6.19 Patches](changelog_4_6.md#idp50370888) </span>

<span class="sect2"> [Database or Log File On-Disk Format Changes:](changelog_4_6.md#idp50361912) </span>

<span class="sect2"> [New Features:](changelog_4_6.md#idp50454856) </span>

<span class="sect2"> [Database Environment Changes:](changelog_4_6.md#idp50457960) </span>

<span class="sect2"> [Concurrent Data Store Changes:](changelog_4_6.md#idp50459800) </span>

<span class="sect2"> [General Access Method Changes:](changelog_4_6.md#idp50458344) </span>

<span class="sect2"> [Btree Access Method Changes:](changelog_4_6.md#idp50475672) </span>

<span class="sect2"> [Hash Access Method Changes:](changelog_4_6.md#idp50460536) </span>

<span class="sect2"> [Queue Access Method Changes:](changelog_4_6.md#idp50444272) </span>

<span class="sect2"> [Recno Access Method Changes:](changelog_4_6.md#idp50463616) </span>

<span class="sect2"> [C++-specific API Changes:](changelog_4_6.md#idp50463872) </span>

<span class="sect2"> [Java-specific API Changes:](changelog_4_6.md#idp50481800) </span>

<span class="sect2"> [Java collections and bind API Changes:](changelog_4_6.md#idp50464456) </span>

<span class="sect2"> [Tcl-specific API Changes:](changelog_4_6.md#idp50464944) </span>

<span class="sect2"> [RPC-specific Client/Server Changes:](changelog_4_6.md#idp50465232) </span>

<span class="sect2"> [Replication Changes:](changelog_4_6.md#idp50486584) </span>

<span class="sect2"> [XA Resource Manager Changes:](changelog_4_6.md#idp50466136) </span>

<span class="sect2"> [Locking Subsystem Changes:](changelog_4_6.md#idp50465496) </span>

<span class="sect2"> [Logging Subsystem Changes:](changelog_4_6.md#idp50451848) </span>

<span class="sect2"> [Memory Pool Subsystem Changes:](changelog_4_6.md#idp50452712) </span>

<span class="sect2"> [Transaction Subsystem Changes:](changelog_4_6.md#idp50468064) </span>

<span class="sect2"> [Utility Changes:](changelog_4_6.md#idp50475736) </span>

<span class="sect2"> [Configuration, Documentation, Portability and Build Changes:](changelog_4_6.md#idp50479800) </span>

### 4.6.21 Patches:

1.  Fix a bug where mutex contention in database environments configured for hybrid mutex support could result in performance degradation. \[#15646\]
2.  Fix a bug where closing a database handle after aborting a transaction which included a failed open of that database handle could result in application failure. \[#15650\]
3.  Fix multiple MVCC bugs including a race which <span class="emphasis">*could result in incorrect data being returned*</span> to the application. \[#15653\]
4.  Fix a bug where a database store into a Hash database could self-deadlock in a database environment configured for the Berkeley DB Concurrent Data Store product and with a free-threaded DB_ENV or DB handle. \[#15718\]
5.  Fix an installation bug where Berkeley DB's PHP header file was not installed in the correct place.

### 4.6.19 Patches

1.  Fix a bug where a client in a two-site replication group could become master, after failure of the existing master, even if the client had priority 0. \[#15388\]
2.  Fix a bug where 32-bit builds on 64-bit machines could immediately core dump because of a misaligned access. \[#15643\]
3.  Fix a bug where attempts to configure a database for MVCC in the Java API were silently ignored. \[#15644\]
4.  Fix a bug where database environments configured for replication and verbose output could drop core. \[#15651\]

### Database or Log File On-Disk Format Changes:

1.  The on-disk log format has changed.
2.  The format of Hash database pages was changed in the Berkeley DB 4.6 release, and items are now stored in sorted order. <span class="emphasis">*The format changes are entirely backward-compatible, and no database upgrades are needed.*</span> However, upgrading existing databases can offer significant performance improvements. Note that databases created using the 4.6 release may not be usable with earlier Berkeley DB releases.

### New Features:

1.  Add support for a cursor DB_PREV_DUP flag, which moves the cursor to the previous key/data pair if it's a duplicate of the current key/data pair. \[#4801\]
2.  Add the ability to set cache page priority on a database or cursor handle. \[#11886\]
3.  Add verbose output tracing for filesystem operations. \[#13760\]
4.  Port Berkeley DB to Qualcomm's Binary Runtime Environment for Wireless (BREW). \[#14562\]
5.  Port Berkeley DB to WinCE. \[#15312\]
6.  Port Berkeley DB to S60. \[#15371\]
7.  Add a key_exists method to the DB handle. \[#15374\]
8.  Applications may now begin processing new transactions while previously prepared, but unresolved, transactions are still pending. \[#14754\]
9.  Significant performance improvements in the Hash access method. \[#15017\]

### Database Environment Changes:

1.  Add support to close open file handles in the case of catastrophic database environment failure so applications that do not exit and restart on failure won't leak file handles. \[#6538\]
2.  Replace the Berkeley DB shared memory allocator with a new implementation, intended to decrease the performance drop-off seen in database environments having working sets that are larger than the cache, especially database environments with multiple cache page sizes. \[#13122\]
3.  Fix a bug that would incorrectly cause a thread to appear to be in the Berkeley DB API after a call to db_create. \[#14562\]
4.  Allow database close prior to resolving all transactions updating the database. \[#14785\]
5.  Fix a bug where the db_stat utility -Z flag and the statistics method's DB_STAT_CLEAR flag could clear mutex statistics too quickly, leading to incorrect values being displayed. \[#15032\]
6.  Fix a bug where removal of a file after and open/close pair spanning the most recent checkpoint log-sequence-numbers made recovery fail. \[#15092\]
7.  Fix a bug that could leave an environment unrecoverable if FTRUNCATE was not set and a roll-forward to a timestamp was interrupted between the truncation of the log and the recording of aborted allocations. \[#15108\]
8.  Fix a bug where recovery of a rename operation could fail if the rename occurred in a directory that no longer existed. \[#15119\]
9.  Fix a bug that could cause recovery to report a "File exists" error if a committed create was partially recovered by a previously failed recovery operation. \[#15151\]
10. Fix a bug where the DbEnv.get_thread_count method implementation was missing from the Berkeley DB 4.5 release. \[#15201\]
11. Fix a bug where replication operations were not reported properly when the DbEnv.failchk method was called. \[#15094\]
12. Fixed a bug that caused SEQ-\>remove not to use a transaction if the sequence was opened on a transactional database handle but no transaction was specified on the call. \[#15235\]
13. Fix a bug where accesses to the database environment reference count could race, causing the DB_ENV-\>remove method to incorrectly remove or not remove a database environment. \[#15240\]
14. Fix a bug that could cause a recovery failure if a partial record was written near the end of a log file before a crash and then never overwritten after recovery runs and before a log file switch occurs. \[#15302\]
15. Fix a bug that could fire a diagnostic assertion if an error occurred during a database environment open. \[#15309\]
16. Fix a bug where memp_trickle attempts to flush an infinite number of buffers. \[#15342\]
17. Cause application updates of the DB_ENV-\>set_mp_max_write values to affect already running cache flush operations. \[#15342\]
18. Fix a bug which could cause system hang if a checkpoint happened at the same time as a database file create or rename. \[#15346\]
19. Fix a bug which could cause application failure if the open of a subdatabase failed while other database opens were happening. \[#15346\]
20. Fix a bug that could cause recovery to not process a transaction properly if the transaction was started before the transaction IDs were reset but did not put its first record into the log until after the txn_recycle record. \[#15400\]
21. Fix a bug that could cause a thread in cache allocation to loop infinitely. \[#15406\]
22. Fix a bug that could cause recovery to report a Log Sequence Error on systems without the ftruncate system call where a page allocation occurred and the database metadata page was forced out of cache without being marked dirty and then had to be recovered. \[#15441\]
23. Fix a bug on systems lacking the ftruncate system call, where a page may be improperly linked into the free list if archive recovery was done in multiple steps, that is, applying additional logs to the same databases. \[#15557\]

### Concurrent Data Store Changes:

None.

### General Access Method Changes:

1.  Add a feature where applications can specify a custom comparison function for the Hash access method \[#4109\]
2.  Open, create, close and removal of non-transactional databases is are longer logged in transactional database environments unless debug logging is enabled. \[#8037\]
3.  Add the ability to set cache page priority on a database or cursor handle. \[#11886\]
4.  fix a bug where the DB_ENV-\>fileid_reset method failed when called on on encrypted or check-summed databases. \[#13990\]
5.  Fix a bug where the DB-\>fd method failed when called on in-memory databases. \[#14157\]
6.  Fix a bug where an attempt to open a Recno database with a backing file that does not exist could report an error because it couldn't remove a temporary file. \[#14160\]
7.  Reverse a change found in previous releases which disallowed setting "partial" flags on key DBTs for DB and DbCursor put method calls. \[#14520\]
8.  Fix a bug where transactional file operations, such as remove or rename, could leak file handles. \[#15222\]
9.  Fix a bug that could cause the in-memory sorted freelist used by the DB-\>compact method not to be freed if transaction or lock timeouts were set in the environment. \[#15292\]
10. Add the DB-\>get_multiple method, which returns if the DB handle references a "master" database in the physical file. \[#15352\]
11. Fix a bug that could cause an DB_INORDER, DB-\>get method DB_CONSUME operation to loop if the Queue database was missing a record due to a rollback by a writer or a non-queue insert in the queue. \[#15452\]
12. Fix a bug preventing database removal after application or system failure in a database environment configured for in-memory logging. \[#15459\]

### Btree Access Method Changes:

None.

### Hash Access Method Changes:

1.  Change the internal format of Hash database pages, storing items in sorted order. There are no externally visible changes, and hash databases using historic on-page formats do not require an explicit upgrade. (However, upgrading existing databases can offer significant performance improvements.) \[#15017\]
2.  Fix a bug preventing LSNs from being reset on hash databases when the databases were configured with a non-standard hash function. \[#15567\]

### Queue Access Method Changes:

1.  Fix a bug which could cause a Queue extent file to be incorrectly removed if an empty extent file was being closed by one thread and being updated by another thread (which was using random access operations). \[#9101\]

### Recno Access Method Changes:

None.

### C++-specific API Changes:

None.

### Java-specific API Changes:

1.  Add a feature where an exception is thrown by the Java API, the Berkeley DB error message is now included in the exception object. \[#11870\]
2.  Fix a bug which can cause a JVM crash when doing a partial get operation. \[#15143\]
3.  Fix a bug which prevented the use of Berkeley DB sequences from Java. \[#15220\]
4.  Fix multiple bugs where DBTs were not being copied correctly in the Java replication APIs. \[#15223\]
5.  Add transaction.commitWriteNoSync to the Java API. \[#15376\]

### Java collections and bind API Changes:

1.  Change SerialBinding to use the current thread's context class loader when loading application classes. This allows the JE jar file to be deployed in application servers and other containers as a shared library rather than as an application jar. \[#15447\]
2.  Tuple bindings now support the java.math.BigInteger type. Like other tuple binding values, BigInteger values are sorted in natural integer order by default, without using a custom comparator. For details please see the Javadoc for: com.sleepycat.bind.tuple.TupleInput.readBigInteger com.sleepycat.bind.tuple.TupleOutput.writeBigInteger com.sleepycat.bind.tuple.BigIntegerBinding \[#15244\]
3.  Add tuple binding methods for reading and writing packed int and long values. Packed integer values take less space, but take slightly more processing time to read and write. See: TupleInput.readPackedInt TupleInput.getPackedIntByteLength TupleInput.readPackedLong TupleInput.getPackedLongByteLength TupleOutput.writePackedInt TupleOutput.writePackedLong PackedInteger \[#15422\]
4.  The Collections API has been enhanced so that auto-commit works for the standard Java Iterator.remove(), set() and add() methods. Previously it was necessary to explicitly begin and commit a transaction in order to call these methods, when the underlying Database was transactional. Note that starting a transaction is still necessary when calling these methods if the StoredCollection.storedIterator method is used. \[#15401\]
5.  Fix a bug that causes a memory leak for applications where both of the following are true: many Environment objects are opened and closed, and the CurrentTransaction or TransactionRunner class is used. \[#15444\]

### Tcl-specific API Changes:

None.

### RPC-specific Client/Server Changes:

None.

### Replication Changes:

1.  Fix a bug where transactions could be rolled-back if an existing replication group master was partitioned and unable to participate in an election. \[#14752\]
2.  Add a new event when a replication manager framework master fails to send and confirm receipt by clients of a "permanent" message. \[#14775\]
3.  Fix a race where multiple threads might attempt to process a LOGREADY condition. \[#14902\]
4.  Change the DB_VERB_REPLICATION flag to no longer require the Berkeley DB library be built with the --enable-diagnostic configuration option to output additional replication logging information. \[#14991\]
5.  Fix a bug with elections occurring during internal init of a replication client site. \[#15057\]
6.  Fix lockout code to lockout message threads and API separately. Send indication that log requests is for internal init. \[#15067\]
7.  Replication manager changed to retry host-name look-up failures, since they could be caused by transient name server outage. \[#15081\]
8.  Fix a bug which led to memory corruption when the sending of a bulk buffer resulted in an error. \[#15100\]
9.  A throttling limit of 10 megabytes is now set by default in a newly created database environment (see the DbEnv.rep_set_limit method). \[#15115\]
10. Fix a bug in ALL_REQ handling where master could get a DB_NOTFOUND. \[#15116\]
11. Fix a bug which could lead to client sites repeatedly but unproductively calling for an election, when a master site already exists. \[#15128\]
12. Modify gap processing algorithms so XXX_MORE messages ask for data beyond what it just processed, not an earlier gap that might exist. \[#15136\]
13. Fixed a bug in the ex_rep example application which could cause the last few transactions to disappear when shutting down the sites of the replication group gracefully. \[#15162\]
14. Fix a bug where if a client crashed during internal init, its database environment would be left in a confused state, making it impossible to synchronize again with the master. \[#15177\]
15. Fix a bug where election flags are not cleared atomically with the setting of the new master ID. \[#15186\]
16. Fix a bug which would cause Berkeley DB to crash if an internal init happened when there were no database files at the master. \[#15227\]
17. It is now guaranteed that the DB_EVENT_REP_STARTUPDONE event will be presented to the application after the corresponding DB_EVENT_REP_NEWMASTER event, even in the face of extreme scheduling anomalies. \[#15265\]
18. Fix minor memory leaks in the replication manager. \[#15239\] \[#15256\]
19. Fix a bug which caused the replication manager to lose track of a failed connection, resulting in the inability to accept a replacement connection. \[#15311\]
20. Fix a bug where a client starting an election when the rest of the replication group already had an established master could confuse replication management at the other client sites, leading to failure to properly acknowledge PERM transactions from the master. \[#15428\]
21. Add support for reporting Replication Manager statistics. \[#15430\]
22. Fix a bug where a send failure during processing of a request message from a client could erroneously appear to the application as an EPERM system error. \[#15436\]
23. Client now sets STARTUPDONE at the end of the synchronization phase when it has caught up to the end of the master's transaction log, without requiring ongoing transactions at the master. \[#15542\]
24. Fix a bug in sleep-time calculation which could cause a Replication Manager failure. \[#15552\]

### XA Resource Manager Changes:

None.

### Locking Subsystem Changes:

1.  Change the DB_ENV-\>lock_detect method to return the number of transactions timed out in addition to those were rejected due to deadlock. \[#15281\]

### Logging Subsystem Changes:

None.

### Memory Pool Subsystem Changes:

1.  Fix a bug that could cause a checkpoint to hang if a database was closed while the checkpoint was forcing that file to disk and all the pages for that database were replaced in the cache. \[#15135\]
2.  Fix a bug where a system error in closing a file could result in a core dump. \[#15137\]
3.  Fix MVCC statistics counts for private database environments. \[#15218\]

### Transaction Subsystem Changes:

1.  Fix a bug where creating a database with the DB_TXN_NOTDURABLE flag set would still write a log record. \[#15386\]
2.  Change transaction checkpoint to wait only for pages being updated during the checkpoint. \[#14710\]

### Utility Changes:

1.  Fix a bug that prevented db_load from handling subdatabase names that were of zero length. \[#8204\]
2.  Fix a bug where the db_hotbackup utility did not clean out and record the log file numbers in the backup directory when both the -u and -D flags were specified. \[#15395\]

### Configuration, Documentation, Portability and Build Changes:

1.  Berkeley DB no longer supports process-shared database environments on Windows 9X platforms; the DB_PRIVATE flag must always be specified to the DB_ENV-\>open method. \[#13766\]
2.  Port Berkeley DB to Qualcomm's Binary Runtime Environment for Wireless (BREW). \[#14562\]
3.  Compile SWIG-generated code with the -fno-strict-aliasing flag when using the GNU gcc compiler. \[#14953\]
4.  Changed include files so ENOENT is resolved on Windows. \[#15078\]
5.  Port Berkeley DB to WinCE. \[#15312\]
6.  Port Berkeley DB to S60. \[#15371\]
7.  Add the db_hotbackup executable to the Windows MSI installer. \[#15372\]
8.  Change the db_hotbackup utility to use the Berkeley DB library portability layer. \[#15415\]
9.  Re-write the GNU gcc mutex implementation on the x86 platform to avoid compiler errors. \[#15461\]
10. Fix a bug with non-HFS filesystems under OS X which could affect data durability. \[#15501\]
