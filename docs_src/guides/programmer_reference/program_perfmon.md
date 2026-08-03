---
title: "Performance Event Monitoring"
api-name: "Performance Event Monitoring"
source: docs/programmer_reference/program_perfmon.html
---
## Performance Event Monitoring

<span class="sect2"> [Using the DTrace Provider](program_perfmon.md#program_perfmon_dtrace) </span>

<span class="sect2"> [Using SystemTap](program_perfmon.md#program_perfmon_stap) </span>

<span class="sect2"> [Example Scripts](program_perfmon.md#program_perfmon_examples) </span>

<span class="sect2"> [Performance Events Reference](program_perfmon.md#program_perfmon_probes) </span>

The Performance Event Monitoring feature uses Solaris DTrace or Linux SystemTap to "publish" interesting events as they occur inside of Berkeley DB. The operating system utilities <span class="command">**dtrace**</span> or <span class="command">**stap**</span> run scripts which select, analyze, and display events. There is no need to modify the application. Any application which uses that Berkeley DB library can be monitored. For more information about these instrumentation tools refer to the following pages:

<span class="term">DTrace</span>  
<a href="http://www.oracle.com/technetwork/systems/dtrace/dtrace/index-jsp-137532.html" class="ulink" target="_top">http://www.oracle.com/technetwork/systems/dtrace/dtrace/index-jsp-137532.html</a>

<span class="term">SystemTap</span>  
<a href="http://sourceware.org/systemtap/" class="ulink" target="_top">http://sourceware.org/systemtap/</a>

Performance Event Monitoring is available for operating systems where DTrace or SystemTap supports static probe points in user applications, such as Solaris 10 and OpenSolaris, some versions of Linux, and Mac OS X 10.6 and later. By including --enable-dtrace to the configuration options, the resulting libraries will include probe points for these event categories:

- database operations: opening a database or cursor, get, put, delete.
- internal operations regarding disk allocation, transactions, concurrency control, and caching.
- the beginning and ending of waiting periods due to conflicting transactional consistency locks (for example, page locks), mutexes, or shared latches.
- timing dependent code paths which are expected to be infrequent. These could raise concerns if they were to happen too often.

These probe points are implemented as user-level statically defined traces (USDT's) for DTrace, and static userspace markers for SystemTap.

To monitor the statistics values as they are updated include the --enable-perfmon-statistics configuration option. This option generates probe points for updates to many of the counters whose values are displayed by the db_stat utility or returned by the various statistics functions. The "cache" example script uses a few of these probe points.

Performance Event Monitoring is intended to be suitable for production applications. Running Berkeley DB with DTrace or SystemTap support built in has little effect on execution speed until probes are enabled at runtime by the <span class="command">**dtrace**</span> or <span class="command">**stap**</span> programs.

The list of available events may be displayed by running 'make listprobes' after building the libdb-5.3 shared library.

### Using the DTrace Provider

The DTrace probe provider for Berkeley DB is named 'bdb'. A simple dtrace command to monitor all dtrace-enabled Berkeley DB activity in the system is:

` dtrace -Zn 'bdb*::: { printf("%s", probename); }' `

DTrace requires elevated privileges in order to run. On Solaris you can avoid running as root by giving any users who need to run <span class="command">**dtrace**</span> the dtrace_proc or dtrace_user privilege in <span class="command">**/etc/user_attr**</span>.

DTrace works on both 32 and 64 bit applications. However, when tracing a 32-bit application on a 64-bit processor it might be necessary to pass a "32 bit" option to <span class="command">**dtrace**</span>. Without this option the D language might use a pointer size of 8 bytes, which could cause pointer values (and structures containing them) to be processed incorrectly. Use the `-32` option on Solaris; on Mac OS X use `-arch i386`.

### Using SystemTap

SystemTap looks up its static userspace markers in the library name specified in the <span class="command">**stap**</span> script. A simple <span class="command">**stap**</span> command to list the probes of the default Berkeley DB installation is:

` stap -l 'process("/usr/local/BerkeleyDB.5.3/lib/libdb-5.3.so").mark("*")' `

Berkeley DB supports SystemTap version 1.1 or later. Building with userspace marker support requires `sys/sdt.h`, which is often available in the package `systemtap-sdt-devel`. Running <span class="command">**stap**</span> with userspace markers requires that the kernel have "utrace" support; see <a href="http://sourceware.org/systemtap/wiki/utrace" class="ulink" target="_top">http://sourceware.org/systemtap/wiki/utrace</a> for more information.

SystemTap needs elevated privileges in order to run. You can avoid running as root by adding the users who need to run <span class="command">**stap**</span> to the group stapdev.

### Example Scripts

Berkeley DB includes several example scripts, in both DTrace and SystemTap versions. The DTrace examples end with a `.d` suffix and are located in `util/dtrace`. The SystemTap examples have a `.stp` suffix and can found be in `util/systemtap`. The Berkeley DB shared library name, including any necessary path, is expected as the first parameter to the SystemTap examples.

<span class="term">apicalls</span>  
This script graphs the count of the main API calls. The result is grouped by thread of the target process.

<span class="term">apitimes</span>  
This script graphs the time spent in the main API calls, grouped by thread.

<span class="term"> apitrace </span>  
This script displays the entry to and return from each of the main API calls.

<span class="term"> cache </span>  
This script displays overall and per-file buffer cache statistics every <span class="emphasis">*N*</span> (default: 1) seconds for <span class="emphasis">*M*</span> (default: 60) intervals. It prints the number of cache hits, misses, and evictions for each file with any activity during the interval.

<span class="term"> dbdefs</span>  
This contains DTrace-compatible declarations of Berkeley DB data structures returned from probe points. There is no Linux equivalent; SystemTap can obtain type information directly from the debugging symbols compiled into the libdb\*-5.3.so shared library.

<span class="term"> locktimes </span>  
This script graphs the time spent waiting for DB page locks. The result times in nanoseconds are grouped by filename, pgno, and lock_mode. The optional integer maxcount parameter directs the script to exit once that many page lock waits have been measured.

<span class="term"> locktimesid </span>  
This is similar to the locktimes script above, except that it displays the 20 byte file identifier rather than the file name. This can be useful when there are several environments involved, or when database files are recreated during the monitoring period.

<span class="term"> mutex </span>  
This script measures mutex wait periods, summarizing the results two ways.

- The first grouping is by mutex, mode (exclusive or shared), and thread id.
- The second grouping is by the mutex category and mode (exclusive or shared). The mutex categories are the MTX_XXX definitions in `dbinc/mutex.h`.

<span class="term"> showerror </span>  
This script displays the application stack when the basic error routines are called. It provides additional information about an error, beyond the string sent to the diagnostic output.

These examples are designed to trace a single process. They run until interrupted, or the monitored process exits, or the limit as given in an optional 'maximum count' argument has been reached.

### Performance Events Reference

The events are described below as if they were functions with ANSI C-style signatures. The values each event provides to DTrace or SystemTap are the arguments to the functions.

<span class="term"> alloc </span>  
The alloc class covers the allocation of "on disk" database pages.

<span class="term">alloc-new (char \*file, char \*db, unsigned pgno, unsigned type, struct \_db_page \*pg, int ret); </span>  
An attempt to allocate a database page of type 'type' for database 'db' returned 'ret'. If the allocation succeeded then ret is 0, pgno is the location of the new page, and pg is the address of the new page. Details of the page can be extracted from the pg pointer.

<span class="term"> alloc-free (char \*file, char \*db, unsigned pgno, unsigned ret); </span>  
An attempt to free the page 'pgno' of 'db' returned 'ret'. When successful the page is returned to the free list or the file is truncated.

<span class="term">alloc-btree_split (char \*file, char \*db, unsigned pgno, unsigned parent, unsigned level); </span>  
A btree split of pgno in db is being attempted. The parent page number and the level in the btree are also provided.

<span class="term"> db </span>  
These DB API calls provide the name of the file and database being accessed. In-memory databases will have a NULL (0) file name address. The db name will be null unless subdatabases are in use.

<span class="term"> db-open (char \*file, char \*db, unsigned flags, uint8_t \*fileid); </span>  
The database or file name was opened. The 20 byte unique fileid can be used to keep track of databases as they are created and destroyed.

<span class="term"> db-close (char \*file, char \*db, unsigned flags, uint8_t \*fileid); </span>  
The database or file name was closed.

<span class="term">db-cursor (char \*file, char \*db, unsigned txnid, unsigned flags, uint8_t \*fileid); </span>  
An attempt is being made to open a cursor on the database or file.

<span class="term">db-get (char \*file, char \*db, unsigned txnid, DBT \*key, DBT \*data, unsigned flags); </span>  
An attempt is being made to get data from a db.

<span class="term">db-put (char \*file, char \*db, unsigned txnid, DBT \*key, DBT \*data, unsigned flags); </span>  
An attempt is being made to put data to a db.

<span class="term"> db-del (char \*file, char \*db, unsigned txnid, DBT \*key, unsigned flags); </span>  
An attempt is being made to delete data from a db.

<span class="term"> lock </span>  
The lock class monitors the transactional consistency locks: page, record, and database. It also monitors the non-transactional file handle locks.

<span class="term"> lock-suspend (DBT \*lock, db_lockmode_t lock_mode); </span>  
The thread is about to suspend itself because another locker already has a conflicting lock on object 'lock'. The lock DBT's data points to a \_\_db_ilock structure, except for the atypical program which uses application specific locking.

<span class="term"> lock-resume (DBT \*lock, db_lockmode_t lock_mode); </span>  
The thread is awakening from a suspend.

<span class="term"> lock-put (struct \_\_sh_dbt \*lock, unsigned flags); </span>  
The lock is being freed.

<span class="term"> lock-put_reduce_count (struct \_\_sh_dbt \*lock, unsigned flags); </span>  
The lock would have been freed except that its refcount was greater than 1.

These lock counters are included by --enable-perfmon-statistics.

<span class="term">lock-deadlock (unsigned st_ndeadlocks, unsigned locker_id, struct \_\_sh_dbt \*lock_obj); </span>  
The locker_id's lock request in lock_obj is about to be aborted in order to resolve a deadlock. The lock region's st_ndeadlocks has been incremented.

<span class="term"> lock-nowait_notgranted (unsigned count, DBT \*lock, unsigned locker_id); </span>  
A DB_LOCK_NOWAIT lock request by locker_id would have had to wait. The lock regions's st_lock_nowait has been incremented and the request returns DB_LOCK_NOTGRANTED.

<span class="term"> lock-steal (unsigned st_locksteals, unsigned from, unsigned to); </span>  
A lock is being stolen from one partition for another one. The 'from' lock partition's st_locksteals has been incremented.

<span class="term"> lock-object_steal (unsigned st_objectsteals, unsigned from, unsigned to); </span>  
A lock object is being stolen from one partition for another one. The 'from' lock partition's st_objectsteals has been incremented.

<span class="term"> lock-locktimeout (unsigned st_nlocktimeouts, const DBT \*lock); </span>  
A lock wait expired due to the lock request timeout.

<span class="term"> lock-txntimeout (unsigned st_ntxntimeouts, const DBT \*lock); </span>  
A lock wait expired due to the transaction's timeout.

<span class="term"> lock-nlockers (unsigned active, unsigned locker_id); </span>  
The allocation or deallocation of the locker id changed the number of active locker identifiers.

<span class="term"> lock-maxnlockers (unsigned new_max_active, unsigned locker_id); </span>  
The allocation of the locker id set a new maximum number of active locker identifiers.

<span class="term"> mpool </span>  
The mpool class monitors the allocation and management of memory, including the cache.

<span class="term"> mpool-read (char \*file, unsigned pgno, struct \_\_bh \*buf); </span>  
Read a page from file into buf.

<span class="term"> mpool-write (char \*file, unsigned pgno, struct \_\_bh \*buf); </span>  
Write a page from buf to file.

<span class="term"> mpool-env_alloc (unsigned size, unsigned region_id, unsigned reg_type); </span>  
This is an attempt to allocate size bytes from region_id. The reg_type is one of the reg_type_t enum values.

<span class="term"> mpool-evict (char \*file, unsigned pgno, struct \_\_bh \*buf); </span>  
The page is about to be removed from the cache.

<span class="term">mpool-alloc_wrap (unsigned alloc_len, int region_id, int wrap_count, int put_counter); </span>  
The memory allocator has incremented wrap_count after searching through the entire region without being able to fulfill the request for alloc_len bytes. As wrap_count increases the library makes more effort to allocate space.

These mpool counters are included by --enable-perfmon-statistics.

<span class="term"> mpool-clean_eviction (unsigned st_ro_evict, unsigned region_id); </span>  
The eviction of a clean page from a cache incremented st_ro_evict.

<span class="term"> mpool-dirty_eviction (unsigned st_rw_evict, unsigned region_id); </span>  
The eviction of a dirty page from a cache incremented st_rw_evict. The page has already been written out.

<span class="term"> mpool-fail (unsigned failure_count, unsigned alloc_len, unsigned region_id); </span>  
An attempt to allocate memory from region_id failed.

<span class="term"> mpool-hash_search (unsigned st_hash_searches, char \*file, unsigned pgno); </span>  
A search for pgno of file incremented st_hash_searches.

<span class="term"> mpool-hash_examined (unsigned st_hash_examined, char \*file, unsigned pgno); </span>  
A search for pgno of file increased st_hash_examined by the number of hash buckets examined.

<span class="term"> mpool-hash_longest (unsigned st_hash_longest, char \*file, unsigned pgno); </span>  
A search for pgno of file set a new maximum st_hash_longest value.

<span class="term"> mpool-map (unsigned st_map, char \*file, unsigned pgno); </span>  
A file's st_map count was incremented after a page was mapped into memory. The mapping might have caused disk I/O.

<span class="term"> mpool-hit (unsigned st_cache_hit, char \*file, unsigned pgno); </span>  
The hit count was incremented because pgno from file was found in the cache.

<span class="term"> mpool-miss (unsigned st_cache_miss, char \*file, unsigned pgno); </span>  
The miss count was incremented because pgno from file was not already present in the cache.

<span class="term"> mpool-page_create (unsigned st_page_create, char \*file, unsigned pgno); </span>  
The st_page_create field was incremented because the pgno of file was created in the cache.

<span class="term"> mpool-page_in (unsigned st_page_in, char \*file, unsigned pgno); </span>  
The st_page_in field was incremented because the pgno from file was read into the cache.

<span class="term"> mpool-page_out (unsigned st_page_out, char \*file, unsigned pgno); </span>  
The st_page_out field was incremented because the pgno from file was written out.

<span class="term"> mutex </span>  
The mutex category monitors includes shared latches. The alloc_id value is one of the MTX_XXX definitions from dbinc/mutex.h

<span class="term">mutex-suspend (unsigned mutex, unsigned excl, unsigned alloc_id, struct \_\_db_mutex_t \*mutexp); </span>  
This thread is about to suspend itself because a thread has the mutex or shared latch locked in a mode which conflicts with the this request.

<span class="term">mutex-resume (unsigned mutex, unsigned excl, unsigned alloc_id, struct \_\_db_mutex_t \*mutexp); </span>  
The thread is returning from a suspend and will attempt to obtain the mutex or shared latch again. It might need to suspend again.

These mutex counters are included by --enable-perfmon-statistics.

<span class="term"> mutex-set_nowait (unsigned mutex_set_nowait, unsigned mutex); </span>  
Increment the count of times that the mutex was free when trying to lock it.

<span class="term"> mutex-set_wait (unsigned mutex_set_wait, unsigned mutex); </span>  
Increment the count of times that the mutex was busy when trying to lock it.

<span class="term"> mutex-set_rd_nowait (unsigned mutex_set_rd_nowait, unsigned mutex); </span>  
Increment the count of times that the shared latch was free when trying to get a shared lock on it.

<span class="term"> mutex-set_rd_wait (unsigned mutex_set_rd_wait, unsigned mutex); </span>  
Increment the count of times that the shared latch was already exclusively latched when trying to get a shared lock on it.

<span class="term"> mutex-hybrid_wait (unsigned hybrid_wait, unsigned mutex); </span>  
Increment the count of times that a hybrid mutex had to block on its condition variable. n a busy system this might happen several times before the corresponding hybrid_wakeup.

<span class="term"> mutex-hybrid_wakeup (unsigned hybrid_wakeup, unsigned mutex); </span>  
Increment the count of times that a hybrid mutex finished one or more waits for its condition variable.

<span class="term"> txn </span>  
The txn category covers the basic transaction operations.

<span class="term"> txn-begin (unsigned txnid, unsigned flags); </span>  
A transaction was successfully begun.

<span class="term"> txn-commit (unsigned txnid, unsigned flags); </span>  
A transaction is starting to commit.

<span class="term"> txn-prepare (unsigned txnid, uint8_t \*gid); </span>  
The transaction is starting to prepare, flushing the log so that a future commit can be guaranteed to succeed. The global identifier field is 128 bytes long.

<span class="term"> txn-abort (unsigned txnid); </span>  
The transaction is about to abort.

These txn counters are included by --enable-perfmon-statistics.

<span class="term"> txn-nbegins (unsigned st_nbegins, unsigned txnid); </span>  
Beginning the transaction incremented st_nbegins.

<span class="term"> txn-naborts (unsigned st_nbegins, unsigned txnid); </span>  
Aborting the transaction incremented st_naborts.

<span class="term"> txn-ncommits (unsigned st_ncommits, unsigned txnid); </span>  
Committing the transaction incremented st_ncommits.

<span class="term"> txn-nactive (unsigned st_nactive, unsigned txnid); </span>  
Beginning or ending the transaction updated the number of active transactions.

<span class="term"> txn-maxnactive (unsigned st_maxnactive, unsigned txnid); </span>  
The creation of the transaction set a new maximum number of active transactions.
