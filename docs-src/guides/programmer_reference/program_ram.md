---
title: "Memory-only or Flash configurations"
api-name: "Memory-only or Flash configurations"
source: docs/programmer_reference/program_ram.html
---
## Memory-only or Flash configurations

Berkeley DB supports a variety of memory-based configurations for systems where filesystem space is either limited in availability or entirely replaced by some combination of memory and Flash. In addition, Berkeley DB can be configured to minimize writes to the filesystem when the filesystem is backed by Flash memory.

There are four parts of the Berkeley DB database environment normally written to the filesystem: the database environment region files, the database files, the database environment log files and the replication internal files. Each of these items can be configured to live in memory rather than in the filesystem:

<span class="term">The database environment region files:</span>  
Each of the Berkeley DB subsystems in a database environment is described by one or more regions, or chunks of memory. The regions contain all of the per-process and per-thread shared information (including mutexes), that comprise a Berkeley DB environment. By default, these regions are backed by the filesystem. In situations where filesystem backed regions aren't optimal, applications can create memory-only database environments in two different types of memory: either in the application's heap memory or in system shared memory.

To create the database environment in heap memory, specify the <a href="../../api/c/envopen.md#envopen_DB_PRIVATE" class="olink">DB_PRIVATE</a> flag to the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> method. Note that database environments created in heap memory are only accessible to the threads of a single process, however.

To create the database environment in system shared memory, specify the <a href="../../api/c/envopen.md#envopen_DB_SYSTEM_MEM" class="olink">DB_SYSTEM_MEM</a> flag to the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> method. Database environments created in system memory are accessible to multiple processes, but note that database environments created in system shared memory do create a small (roughly 8 byte) file in the filesystem, read by the processes to identify which system shared memory segments to use.

For more information, see <a href="env_region.md" class="xref" title="Shared memory regions">Shared memory regions</a>.

<span class="term">The database files:</span>  
By default, databases are periodically flushed from the Berkeley DB memory cache to backing physical files in the filesystem. To keep databases from being written to backing physical files, pass the <a href="../../api/c/mempset_flags.md#mpool_set_flags_DB_MPOOL_NOFILE" class="olink">DB_MPOOL_NOFILE</a> flag to the <a href="../../api/c/mempset_flags.md" class="olink">DB_MPOOLFILE-&gt;set_flags()</a> method. This flag implies the application's databases must fit entirely in the Berkeley DB cache, of course. To avoid a database file growing to consume the entire cache, applications can limit the size of individual databases in the cache by calling the <a href="../../api/c/mempset_maxsize.md" class="olink">DB_MPOOLFILE-&gt;set_maxsize()</a> method.

<span class="term">The database environment log files:</span>  
If a database environment is not intended to be transactionally recoverable after application or system failure (that is, if it will not exhibit the transactional attribute of "durability"), applications should not configure the database environment for logging or transactions, in which case no log files will be created. If the database environment is intended to be durable, log files must either be written to stable storage and recovered after application or system failure, or they must be replicated to other systems.

In applications running on systems without any form of stable storage, durability must be accomplished through replication. In this case, database environments should be configured to maintain database logs in memory, rather than in the filesystem, by specifying the <a href="../../api/c/envlog_set_config.md#log_set_config_DB_LOG_IN_MEMORY" class="olink">DB_LOG_IN_MEMORY</a> flag to the <a href="../../api/c/envlog_set_config.md" class="olink">DB_ENV-&gt;log_set_config()</a> method.

<span class="term">The replication internal files:</span>  
By default, Berkeley DB replication stores a small amount of internal data in the filesystem. To store this replication internal data in memory only and not in the filesystem, specify the <a href="../../api/c/repconfig.md#config_DB_REP_CONF_INMEM" class="olink">DB_REP_CONF_INMEM</a> flag to the <a href="../../api/c/repconfig.md" class="olink">DB_ENV-&gt;rep_set_config()</a> method before opening the database environment.

In systems where the filesystem is backed by Flash memory, the number of times the Flash memory is written may be a concern. Each of the four parts of the Berkeley DB database environment normally written to the filesystem can be configured to minimize the number of times the filesystem is written:

<span class="term">The database environment region files:</span>  
On a Flash-based filesystem, the database environment should be placed in heap or system memory, as described previously.

<span class="term">The database files:</span>  
The Berkeley DB library maintains a cache of database pages. The database pages are only written to backing physical files when the application checkpoints the database environment with the <a href="../../api/c/txncheckpoint.md" class="olink">DB_ENV-&gt;txn_checkpoint()</a> method, when database handles are closed with the <a href="../../api/c/dbclose.md" class="olink">DB-&gt;close()</a> method, or when the application explicitly flushes the cache with the <a href="../../api/c/dbsync.md" class="olink">DB-&gt;sync()</a> or <a href="../../api/c/mempsync.md" class="olink">DB_ENV-&gt;memp_sync()</a> methods.

To avoid unnecessary writes of Flash memory due to checkpoints, applications should decrease the frequency of their checkpoints. This is especially important in applications which repeatedly modify a specific database page, as repeatedly writing a database page to the backing physical file will repeatedly update the same blocks of the filesystem.

To avoid unnecessary writes of the filesystem due to closing a database handle, applications should specify the <a href="../../api/c/dbclose.md#dbclose_DB_NOSYNC" class="olink">DB_NOSYNC</a> flag to the <a href="../../api/c/dbclose.md" class="olink">DB-&gt;close()</a> method.

To avoid unnecessary writes of the filesystem due to flushing the cache, applications should not explicitly flush the cache under normal conditions – flushing the cache is rarely if ever needed in a normally-running application.

<span class="term">The database environment log files:</span>  
The Berkeley DB log files do not repeatedly overwrite the same blocks of the filesystem as the Berkeley DB log files are not implemented as a circular buffer and log files are not re-used. For this reason, the Berkeley DB log files should not cause any difficulties for Flash memory configurations.

However, as Berkeley DB does not write log records in filesystem block sized pieces, it is probable that sequential transaction commits (each of which flush the log file to the backing filesystem), will write a block of Flash memory twice, as the last log record from the first commit will write the same block of Flash memory as the first log record from the second commit. Applications not requiring absolute durability should specify the <a href="../../api/c/envset_flags.md#set_flags_DB_TXN_WRITE_NOSYNC" class="olink">DB_TXN_WRITE_NOSYNC</a> or <a href="../../api/c/envset_flags.md#envset_flags_DB_TXN_NOSYNC" class="olink">DB_TXN_NOSYNC</a> flags to the <a href="../../api/c/envset_flags.md" class="olink">DB_ENV-&gt;set_flags()</a> method to avoid this overwrite of a block of Flash memory.

<span class="term">The replication internal files:</span>  
On a Flash-based filesystem, the replication internal data should be stored in memory only, as described previously.
