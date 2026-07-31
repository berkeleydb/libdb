---
title: "Transaction tuning"
api-name: "Transaction tuning"
source: docs/programmer_reference/transapp_tune.html
---
## Transaction tuning

There are a few different issues to consider when tuning the performance of Berkeley DB transactional applications. First, you should review <a href="am_misc_tune.md" class="xref" title="Access method tuning">Access method tuning</a>, as the tuning issues for access method applications are applicable to transactional applications as well. The following are additional tuning issues for Berkeley DB transactional applications:

<span class="term">access method</span>  
Highly concurrent applications should use the Queue access method, where possible, as it provides finer-granularity of locking than the other access methods. Otherwise, applications usually see better concurrency when using the Btree access method than when using either the Hash or Recno access methods.

<span class="term">record numbers</span>  
Using record numbers outside of the Queue access method will often slow down concurrent applications as they limit the degree of concurrency available in the database. Using the Recno access method, or the Btree access method with retrieval by record number configured can slow applications down.

<span class="term">Btree database size</span>  
When using the Btree access method, applications supporting concurrent access may see excessive numbers of deadlocks in small databases. There are two different approaches to resolving this problem. First, as the Btree access method uses page-level locking, decreasing the database page size can result in fewer lock conflicts. Second, in the case of databases that are cyclically growing and shrinking, turning off reverse splits (with <a href="../../api/c/dbset_flags.md#dbset_flags_DB_REVSPLITOFF" class="olink">DB_REVSPLITOFF</a>) can leave the database with enough pages that there will be fewer lock conflicts.

<span class="term">read locks</span>  
Performing all read operations outside of transactions or at <a href="transapp_read.md" class="xref" title="Degrees of isolation">Degrees of isolation</a> can often significantly increase application throughput. In addition, limiting the lifetime of non-transactional cursors will reduce the length of times locks are held, thereby improving concurrency.

<span class="term"><a href="../../api/c/envset_flags.md#set_flags_DB_DIRECT_DB" class="olink">DB_DIRECT_DB</a>, <a href="../../api/c/envlog_set_config.md#log_set_config_DB_LOG_DIRECT" class="olink">DB_LOG_DIRECT</a></span>  
On some systems, avoiding caching in the operating system can improve write throughput and allow the creation of larger Berkeley DB caches.

<span class="term"><a href="../../api/c/dbopen.md#dbopen_DB_READ_UNCOMMITTED" class="olink">DB_READ_UNCOMMITTED</a>, <a href="../../api/c/dbcget.md#dbcget_DB_READ_COMMITTED" class="olink">DB_READ_COMMITTED</a></span>  
Consider decreasing the level of isolation of transaction using the <a href="../../api/c/dbopen.md#dbopen_DB_READ_UNCOMMITTED" class="olink">DB_READ_UNCOMMITTED</a>, or <a href="../../api/c/dbcget.md#dbcget_DB_READ_COMMITTED" class="olink">DB_READ_COMMITTED</a> flags for transactions or cursors or the <a href="../../api/c/dbopen.md#dbopen_DB_READ_UNCOMMITTED" class="olink">DB_READ_UNCOMMITTED</a> flag on individual read operations. The <a href="../../api/c/dbcget.md#dbcget_DB_READ_COMMITTED" class="olink">DB_READ_COMMITTED</a> flag will release read locks on cursors as soon as the data page is nolonger referenced. This is also called <span class="emphasis"> *degree 2 isolation*</span>. This will tend to block write operations for shorter periods for applications that do not need to have repeatable reads for cursor operations.

The <a href="../../api/c/dbopen.md#dbopen_DB_READ_UNCOMMITTED" class="olink">DB_READ_UNCOMMITTED</a> flag will allow read operations to potentially return data which has been modified but not yet committed, and can significantly increase application throughput in applications that do not require data be guaranteed to be permanent in the database. This is also called <span class="emphasis">*degree 1 isolation*</span>, or <span class="emphasis">*dirty reads*</span>.

<span class="term"> <a href="../../api/c/dbcget.md#dbcget_DB_RMW" class="olink">DB_RMW</a> </span>  
If there are many deadlocks, consider using the <a href="../../api/c/dbcget.md#dbcget_DB_RMW" class="olink">DB_RMW</a> flag to immediately acquire write locks when reading data items that will subsequently be modified. Although this flag may increase contention (because write locks are held longer than they would otherwise be), it may decrease the number of deadlocks that occur.

<span class="term"><a href="../../api/c/envset_flags.md#set_flags_DB_TXN_WRITE_NOSYNC" class="olink">DB_TXN_WRITE_NOSYNC</a>, <a href="../../api/c/envset_flags.md#envset_flags_DB_TXN_NOSYNC" class="olink">DB_TXN_NOSYNC</a></span>  
By default, transactional commit in Berkeley DB implies durability, that is, all committed operations will be present in the database after recovery from any application or system failure. For applications not requiring that level of certainty, specifying the <a href="../../api/c/envset_flags.md#envset_flags_DB_TXN_NOSYNC" class="olink">DB_TXN_NOSYNC</a> flag will often provide a significant performance improvement. In this case, the database will still be fully recoverable, but some number of committed transactions might be lost after application or system failure.

<span class="term">access databases in order</span>  
When modifying multiple databases in a single transaction, always access physical files and databases within physical files, in the same order where possible. In addition, avoid returning to a physical file or database, that is, avoid accessing a database, moving on to another database and then returning to the first database. This can significantly reduce the chance of deadlock between threads of control.

<span class="term">large key/data items</span>  
Transactional protections in Berkeley DB are guaranteed by before and after physical image logging. This means applications modifying large key/data items also write large log records, and, in the case of the default transaction commit, threads of control must wait until those log records have been flushed to disk. Applications supporting concurrent access should try and keep key/data items small wherever possible.

<span class="term">mutex selection</span>  
During configuration, Berkeley DB selects a mutex implementation for the architecture. Berkeley DB normally prefers blocking-mutex implementations over non-blocking ones. For example, Berkeley DB will select POSIX pthread mutex interfaces rather than assembly-code test-and-set spin mutexes because pthread mutexes are usually more efficient and less likely to waste CPU cycles spinning without getting any work accomplished.

For some applications and systems (generally highly concurrent applications on large multiprocessor systems), Berkeley DB makes the wrong choice. In some cases, better performance can be achieved by configuring with the <a href="../../guides/installation/build_unix_conf.md#build_unix_conf.--with-mutex" class="olink">--with-mutex</a> argument and selecting a different mutex implementation than the one selected by Berkeley DB. When a test-and-set spin mutex implementation is selected, it may be useful to tune the number of spins made before yielding the processor and sleeping. For more information, see the <a href="../../api/c/mutexset_tas_spins.md" class="olink">DB_ENV-&gt;mutex_set_tas_spins()</a> method.

Finally, Berkeley DB may put multiple mutexes on individual cache lines. When tuning Berkeley DB for large multiprocessor systems, it may be useful to tune mutex alignment using the <a href="../../api/c/mutexset_align.md" class="olink">DB_ENV-&gt;mutex_set_align()</a> method.

<span class="term"> <a href="../../guides/installation/build_unix_conf.md#build_unix_conf.--enable-posixmutexes" class="olink">--enable-posix-mutexes</a> </span>  
By default, the Berkeley DB library will only select the POSIX pthread mutex implementation if it supports mutexes shared between multiple processes. If your application does not share its database environment between processes and your system's POSIX mutex support was not selected because it did not support inter-process mutexes, you may be able to increase performance and transactional throughput by configuring with the <a href="../../guides/installation/build_unix_conf.md#build_unix_conf.--enable-posixmutexes" class="olink">--enable-posix-mutexes</a> argument.

<span class="term">log buffer size</span>  
Berkeley DB internally maintains a buffer of log writes. The buffer is written to disk at transaction commit, by default, or, whenever it is filled. If it is consistently being filled before transaction commit, it will be written multiple times per transaction, costing application performance. In these cases, increasing the size of the log buffer can increase application throughput.

<span class="term">log file location</span>  
If the database environment's log files are on the same disk as the databases, the disk arms will have to seek back-and-forth between the two. Placing the log files and the databases on different disk arms can often increase application throughput.

<span class="term">trickle write</span>  
In some applications, the cache is sufficiently active and dirty that readers frequently need to write a dirty page in order to have space in which to read a new page from the backing database file. You can use the <a href="../../api/c/db_stat.md" class="olink">db_stat</a> utility (or the statistics returned by the <a href="../../api/c/mempstat.md" class="olink">DB_ENV-&gt;memp_stat()</a> method) to see how often this is happening in your application's cache. In this case, using a separate thread of control and the <a href="../../api/c/memptrickle.md" class="olink">DB_ENV-&gt;memp_trickle()</a> method to trickle-write pages can often increase the overall throughput of the application.
