---
title: "Berkeley DB 4.7.25 Change Log"
api-name: "Berkeley DB 4.7.25 Change Log"
source: docs/upgrading/changelog_4_7.html
---
## Berkeley DB 4.7.25 Change Log

<span class="sect2"> [Database or Log File On-Disk Format Changes:](changelog_4_7.md#idp50357648) </span>

<span class="sect2"> [New Features:](changelog_4_7.md#idp50378912) </span>

<span class="sect2"> [Database Environment Changes:](changelog_4_7.md#idp50380752) </span>

<span class="sect2"> [Concurrent Data Store Changes:](changelog_4_7.md#idp50382592) </span>

<span class="sect2"> [General Access Method Changes:](changelog_4_7.md#idp50381120) </span>

<span class="sect2"> [Btree Access Method Changes:](changelog_4_7.md#idp50391248) </span>

<span class="sect2"> [Hash Access Method Changes:](changelog_4_7.md#idp50365280) </span>

<span class="sect2"> [Queue Access Method Changes:](changelog_4_7.md#idp50355136) </span>

<span class="sect2"> [Recno Access Method Changes:](changelog_4_7.md#idp50346288) </span>

<span class="sect2"> [C-specific API Changes:](changelog_4_7.md#idp50346816) </span>

<span class="sect2"> [Java-specific API Changes:](changelog_4_7.md#idp50347072) </span>

<span class="sect2"> [Direct Persistence Layer (DPL), Bindings and Collections API:](changelog_4_7.md#idp50347296) </span>

<span class="sect2"> [Tcl-specific API Changes:](changelog_4_7.md#idp50386200) </span>

<span class="sect2"> [RPC-specific Client/Server Changes:](changelog_4_7.md#idp50395072) </span>

<span class="sect2"> [Replication Changes:](changelog_4_7.md#idp50395328) </span>

<span class="sect2"> [XA Resource Manager Changes:](changelog_4_7.md#idp50391504) </span>

<span class="sect2"> [Locking Subsystem Changes:](changelog_4_7.md#idp50357904) </span>

<span class="sect2"> [Logging Subsystem Changes:](changelog_4_7.md#idp50397528) </span>

<span class="sect2"> [Memory Pool Subsystem Changes:](changelog_4_7.md#idp50385512) </span>

<span class="sect2"> [Mutex Subsystem Changes:](changelog_4_7.md#idp50386616) </span>

<span class="sect2"> [Transaction Subsystem Changes:](changelog_4_7.md#idp50391632) </span>

<span class="sect2"> [Utility Changes:](changelog_4_7.md#idp50396200) </span>

<span class="sect2"> [Configuration, Documentation, Sample Application, Portability and Build Changes:](changelog_4_7.md#idp50412288) </span>

### Database or Log File On-Disk Format Changes:

1.  The log file format changed in 4.7.

### New Features:

1.  The lock manager may now be fully partitioned, improving performance on some multi-CPU systems. \[#15880\]
2.  Replication groups are now architecture-neutral, supporting connections between differing architectures (big-endian or little-endian, independent of structure padding). \[#15787\] \[#15840\]
3.  Java: A new Direct Persistence Layer adds a built-in Plain Old Java Object (POJO)-based persistent object model, which provides support for complex object models without compromises in performance. For an introduction to the Direct Persistence Layer API, see Getting Started with Data Storage. \[#15936\]
4.  Add the DB_ENV-\>set_intermediate_dir_mode method to support the creation of intermediate directories needed during recovery. \[#15097\]
5.  The DB_ENV-\>failchk method can now abort transactions for threads, which have failed while blocked on a concurrency lock. This significantly decreases the need for database environment recovery after thread of control failure. \[#15626\]
6.  Replication Manager clients now can be configured to monitor the connection to the master using heartbeat messages, in order to promptly discover connection failures. \[#15714\]
7.  The logging system may now be configured to pre-zero log files when they are created, improving performance on some systems. \[#15758\]

### Database Environment Changes:

1.  Restructure aborted page allocation handling on systems without an ftruncate system call. This enables the Berkeley DB High Availability product on systems, which do not support ftruncate. \[#15602\]
2.  Fix a bug where closing a database handle after aborting a transaction which included a failed open of that handle could result in application failure. \[#15650\]
3.  Fix minor memory leaks when closing a private database environment. \[#15663\]
4.  Fix a bug leading to a panic of "unpinned page returned" if a cursor was used for a delete multiple times and deadlocked during one of the deletes. \[#15944\]
5.  Optionally signal processes still running in the environment before running recovery. \[#15984\]

### Concurrent Data Store Changes:

None.

### General Access Method Changes:

1.  Fix a bug where closing a database handle after aborting a transaction which included a failed open of that database handle could result in application failure. \[#15650\]
2.  Fix a bug that could cause panic in a database environment configured with POSIX-style thread locking, if a database open failed. \[#15662\]
3.  Fix bug in the DB-\>compact method which could cause a panic if a thread was about to release a page while another thread was truncating the database file. \[#15671\]
4.  Fix an obscure case of interaction between a cursor scan and delete that was prematurely returning DB_NOTFOUND. \[#15785\]
5.  Fix a bug in the DB-\>compact method where if read-uncommitted was configured, a reader reading uncommitted data my see an inconsistent entry between when the compact method detects an error and when it aborts the enclosing transaction. \[#15856\]
6.  Fix a bug in the DB-\>compact method where a thread of control mail fail if two threads are compacting the same section of a Recno database. \[#15856\]
7.  Fix a bug in DB-\>compact method, avoid an assertion failure when zero pages can be freed. \[#15965\]
8.  Fix a bug return a non-zero error when DB-\>truncate is called with open cursors. \[#15973\]
9.  Fix a bug add HANDLE_DEAD checking for DB cursors. \[#15990\]
10. Fix a bug to now generate errors when DB_SEQUENCE-\>stat is called without first opening the sequence. \[#15995\]
11. Fix a bug to no longer dereference a pointer into a hash structure, when hash functionality is disabled.  \[#16095\]

### Btree Access Method Changes:

None.

### Hash Access Method Changes:

1.  Fix a bug where a database store into a Hash database could self-deadlock in a database environment configured for the Berkeley DB Concurrent Data Store product, and with a free-threaded DB_ENV or DB handle. \[#15718\]

### Queue Access Method Changes:

1.  Fix a bug that could cause a put or delete of a queue element to return a DB_NOTGRANTED error, if blocked. \[#15933\]

### Recno Access Method Changes:

1.  Expose db_env_set_func_malloc, db_env_set_func_realloc, and db_env_set_func_free through the Windows API for the DB dll. \[#16045\]

### C-specific API Changes:

None.

### Java-specific API Changes:

1.  Fix a bug where enabling MVCC on a database through the Java API was ignored. \[#15644\]
2.  Fixed memory leak bugs in error message buffering in the Java API. \[#15843\]
3.  Fix a bug where Java SecondaryConfig was not setting SecondaryMultiKeyCreator from the underlying db handle \[OTN FORUM\]
4.  Fix a bug so that getStartupComplete will now return a boolean instead of an int. \[#16067\]
5.  Fix a bug in the Java API, where Berkeley DB would hang on exit when using replication. \[#16142\]

### Direct Persistence Layer (DPL), Bindings and Collections API:

1.  A new Direct Persistence Layer adds a built-in Plain Old Java Object (POJO)-based persistent object model, which provides support for complex object models without compromises in performance. For an introduction to the Direct Persistence Layer API, see Getting Started with Data Storage. \[#15936\]
2.  Fixed a bug in the remove method of the Iterator instances returned by the StoredCollection.iterator method in the collections package. This bug caused ArrayIndexOutOfBoundsException in some cases when calling next, previous, hasNext or hasPrevious after calling remove. (Note that this issue does not apply to StoredIterator instances returned by the StoredCollection.storedIterator method.) This bug was reported in this forum thread: <a href="http://forums.oracle.com/forums/thread.jspa?messageID=2187896" class="ulink" target="_top">http://forums.oracle.com/forums/thread.jspa?messageID=2187896</a> \[#15858\]
3.  Fixed a bug in the remove method of the StoredIterator instances returned by StoredCollection.storedIterator method in the collections package. If the sequence of methods next-remove-previous was called, previous would sometimes return the removed record. If the sequence of methods previous-remove-next was called, next would sometimes return the removed record. (Note that this issue does not apply to Iterator instances returned by the StoredCollection.iterator method.) \[#15909\]
4.  Fixed a bug that causes a memory leak for applications where many Environment objects are opened and closed and the CurrentTransaction or TransactionRunner class is used. The problem was reported in this JE Forum thread: <a href="http://forums.oracle.com/forums/thread.jspa?messageID=1782659" class="ulink" target="_top">http://forums.oracle.com/forums/thread.jspa?messageID=1782659</a> \[#15444\]
5.  Added StoredContainer.areKeyRangesAllowed method.  Key ranges and the methods in SortedMap and SortedSet such as subMap and subSet are now explicitly disallowed for RECNO and QUEUE databases -- they are only supported for BTREE databases.  Before, using key ranges in a RECNO or QUEUE database did not work, but was not explicitly prohibited in the Collections API. \[#15936\]

### Tcl-specific API Changes:

1.  The Berkeley DB Tcl API does not attempt to avoid evaluating input as Tcl commands. For this reason, it may be dangerous to pass unreviewed user input through the Berkeley DB Tcl API, as the input may subsequently be evaluated as a Tcl command. To minimize the effectiveness of a Tcl injection attack, the Berkeley DB Tcl API in the 4.7 release routine resets process' effective user and group IDs to the real user and group IDs. \[#15597\]

### RPC-specific Client/Server Changes:

None.

### Replication Changes:

1.  Fix a bug where a master failure resulted in multiple attempts to perform a "fast election"; subsequent elections, when necessary, now use the normal nsites value. \[#15099\]
2.  Replication performance enhancements to speed up failover. \[#15490\]
3.  Fix a bug where replication could self-block in a database environment configured for in-memory logging. \[#15503\]
4.  Fix a bug where replication would attempt to read log file version numbers in a database configured for in-memory logging. \[#15503\]
5.  Fix a bug where log files were not removed during client initialization in a database configured for in-memory logging. \[#15503\]
6.  The 4.7 release no longer supports live replication upgrade from the 4.2 or 4.3 releases, only from the 4.4 and later releases. \[#15602\]
7.  Fix a bug where replication could re-request missing records on every arriving record. \[#15629\]
8.  Change the DB_ENV-\>rep_set_request method to use time, not the number of messages, when re-requesting missed messages on a replication client. \[#15629\]
9.  Fix a minor memory leak on the master when updating a client during internal initialization. \[#15634\]
10. Fix a bug where a client error when syncing with a new replication group master could result in an inability to ever re-join the group. \[#15648\]
11. Change dbenv-\>rep_set_request to use time-based values instead of counters. \[#15682\]
12. Fix a bug where a LOCK_NOTGRANTED error could be returned from the DB_ENV-\>rep_process_message method, instead of being handled internally by replication. \[#15685\]
13. Fix a bug where the Replication Manager would reject a fresh connection from a remote site that had crashed and restarted, displaying the message: "redundant incoming connection will be ignored". \[#15731\]
14. The Replication Manager now supports dynamic negotiation of the best available wire protocol version, on a per-connection basis. \[#15783\]
15. Fix a bug, which could lead to slow performance of internal initialization under the Replication Manager, as evidenced by "queue limit exceeded" messages in verbose replication diagnostic output. \[#15788\]
16. Fix a bug where replication control message were not portable between replication clients with different endian architectures. \[#15793\]
17. Add a configuration option to turn off Replication Manager's special handling of elections in 2-site groups. \[#15873\]
18. Fix a bug making it impossible to call replicationManagerAddRemoteSite in the Java API after having called replicationManagerStart. \[#15875\]
19. Fix a bug where the DB_EVENT_REP_STARTUPDONE event could be triggered too early. \[#15887\]
20. Fix a bug where the rcvd_ts timestamp is reset when the user just changes the threshold. \[#15895\]
21. Fix a bug where the master in a 2-site replication group might wait for client acknowledgement, even when there was no client connected. \[#15927\]
22. Fix a bug, clean up and restart internal init if master log is gone. \[#16006\]
23. Fix a bug, ignore page messages that are from an old internal init. \[#16075\] \[#16059\]
24. Fix a bug where checkpoint records do not indicate a database was a named in-memory database. \[#16076\]
25. Fix a bug with in-memory replication, where we returned with the log region mutex held in an error path, leading to self-deadlock. \[#16088\]
26. Fix a bug which causes the DB_REP_CHECKPOINT_DELAY setting in rep_set_timeout() to be interpreted in seconds, rather than microseconds. \[#16153\]

### XA Resource Manager Changes:

1.  Fix a bug where the DB_ENV-\>failchk method and replication in general could fail in database environments configured for XA. \[#15654\]

### Locking Subsystem Changes:

1.  Fix a bug causing a lock or transaction timeout to not be set properly after the first timeout triggers on a particular lock id. \[#15847\]
2.  Fix a bug that would cause a trap if DB_ENV-\>lock_id_free was passed an invalid locker id. \[#16005\]
3.  Fix a bug when thread tracking is enabled where an attempt is made to release a mutex that is not lock. \[#16011\]

### Logging Subsystem Changes:

1.  Fix a bug, handle zero-length log records doing HA sync with in-memory logs. \[#15838\]
2.  Fix a bug that could cause DB_ENV-\>failcheck to leak log region memory. \[#15925\]
3.  Fix a bug where the abort of a transaction that opened a database could leak log region memory. \[#15953\]
4.  Fix a bug that could leak memory in the DB_ENV-\>log_archive interface if a log file was not found. \[#16013\]

### Memory Pool Subsystem Changes:

1.  Fix multiple MVCC bugs including a race, which could <span class="emphasis">*result in incorrect data being returned*</span> to the application. \[#15653\]
2.  Fixed a bug that left an active file in the buffer pool after a database create was aborted. \[#15918\]
3.  Fix a bug where there could be uneven distribution of pages if a single database and multiple cache regions are configured. \[#16015\]
4.  Fix a bug where DB_MPOOLFILE-\>set_maxsize was dropping the wrong mutex after open. \[#16050\]

### Mutex Subsystem Changes:

1.  Fix a bug where mutex contention in database environments configured for hybrid mutex support could result in performance degradation. \[#15646\]
2.  Set the DB_MUTEX_PROCESS_ONLY flag on all mutexes in private environments, they can't be shared and so we can use the faster, intra-process only mutex implementations \[#16025\]
3.  Fix a bug so that mutexes are now removed from the environment signature if mutexes are disabled. \[#16042\]

### Transaction Subsystem Changes:

1.  Fix a bug that could cause a checkpoint to selfblock attempting to flush a file, when the file handle was closed by another thread during the flush. \[#15692\]
2.  Fix a bug that could cause DB_ENV-\>failcheck to hang if there were pending prepared transactions in the environment. \[#15925\]
3.  Prepared transactions will now use the sync setting from the environment.  Default to flushing the log on commit (was nosync). \[#15995\]
4.  If \_\_txn_getactive fails, we now return with the log region mutex held.  This is not a bus since \_\_txn_getactive cannot really fail.  \[#16088\]

### Utility Changes:

1.  Update db_stat with -x option for mutex stats
2.  Fix an incorrect assumption about buffer size when getting an overflow page in db_verify. \[#16064\]

### Configuration, Documentation, Sample Application, Portability and Build Changes:

1.  Fix an installation bug where the Berkeley DB PHP header file was not installed in the correct place.
2.  Merge the run-time configuration sleep and yield functions. \[#15037\]
3.  Fix Handle_DEAD and other expected replication errors in the C++ sample application ReqQuoteExample.cpp. \[15568\]
4.  Add support for monotonic timers. \[#15670\]
5.  Fix bugs where applications using the db_env_func_map and db_env_func_unmap run-time configuration functions could not join existing database environments, or open multiple DB_ENV handles for a single environment. \[#15930\]
6.  Add documentation about building Berkeley DB for VxWorks 6.x.
7.  Remove the HAVE_FINE_GRAINED_LOCK_MANAGER flag, it is obsolete in 4.7.
8.  Fix a bug in ex_rep, add a missing break which could cause a segment fault.
9.  Fix build warnings from 64 bit Windows build. \[#16029\]
10. Fix an alignment bug on ARM Linux.  Force the assignment to use memcpy.  \[#16125\]
11. Fix a bug in the Windows specific code of ex_sequence.c, where there was an invalide printf specifier.  \[#16131\]
12. Improve the timer in ex_tpcb to use high resolution timers.  \[#16154\]
13. Mention in the documentation that env-\>open() requires DB_THREAD to be specified when using repmgr. \[#16163\]
14. Disable support for mmap on Windows CE.  The only affect is that we do not attempt to mmap small read only databases into the mpool. \[#16169\]
