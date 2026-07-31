---
title: "DB_ENV->set_flags()"
api-name: "DB_ENV->set_flags()"
source: docs/api_reference/C/envset_flags.html
---
## DB_ENV-\>set_flags()

``` c
#include <db.h>

int
DB_ENV->set_flags(DB_ENV *dbenv, u_int32_t flags, int onoff);  
```

Configure a database environment.

The database environment's flag values may also be configured using the environment's <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "set_flags", one or more whitespace characters, and the method flag parameter as a string, and optionally one or more whitespace characters, and the string "on" or "off". If the optional string is omitted, the default is "on"; for example, "set_flags DB_TXN_NOSYNC" or "set_flags DB_TXN_NOSYNC on". Because the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

The `DB_ENV->set_flags()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flags

The **flags** parameter must be set by bitwise inclusively **OR**'ing together one or more of the following values:

- `DB_AUTO_COMMIT`

  If set, <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle operations for which no explicit transaction handle was specified, and which modify databases in the database environment, will be automatically enclosed within a transaction.

  Calling `DB_ENV->set_flags()` with this flag only affects the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle (and any other Berkeley DB handles opened within the scope of that handle). For consistent behavior across the environment, all <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handles opened in the environment must either set this flag or the flag should be specified in the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> configuration file.

  This flag may be used to configure Berkeley DB at any time during the life of the application.

- `DB_CDB_ALLDB`

  If set, Berkeley DB Concurrent Data Store applications will perform locking on an environment-wide basis rather than on a per-database basis.

  Calling `DB_ENV->set_flags()` with the DB_CDB_ALLDB flag only affects the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle (and any other Berkeley DB handles opened within the scope of that handle). For consistent behavior across the environment, all <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handles opened in the environment must either set the DB_CDB_ALLDB flag or the flag should be specified in the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> configuration file.

  The DB_CDB_ALLDB flag may be used to configure Berkeley DB only before the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called.

- `DB_DIRECT_DB`

  Turn off system buffering of Berkeley DB database files to avoid double caching.

  Calling `DB_ENV->set_flags()` with the DB_DIRECT_DB flag only affects the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle (and any other Berkeley DB handles opened within the scope of that handle). For consistent behavior across the environment, all <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handles opened in the environment must either set the DB_DIRECT_DB flag or the flag should be specified in the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> configuration file.

  The DB_DIRECT_DB flag may be used to configure Berkeley DB at any time during the life of the application.

- `DB_HOTBACKUP_IN_PROGRESS`

  Set this flag prior to creating a hot backup of a database environment. If a transaction with the bulk insert optimization enabled (with the <a href="txnbegin.md#txnbegin_DB_TXN_BULK" class="xref"><code class="literal">DB_TXN_BULK</code></a> flag) is in progress, setting the `DB_HOTBACKUP_IN_PROGRESS` flag forces a checkpoint in the environment. After this flag is set in the environment, the bulk insert optimization is disabled, until the flag is reset. Using this protocol allows a hot backup procedure to make a consistent copy of the database even when bulk transactions are ongoing. For more information, see the section on Hot Backup in the <span class="emphasis">*Getting Started With Transaction Processing Guide*</span> and the description of the <a href="txnbegin.md#txnbegin_DB_TXN_BULK" class="xref"><code class="literal">DB_TXN_BULK</code></a> flag in the <a href="txnbegin.md" class="xref" title="DB_ENV-&gt;txn_begin()">DB_ENV-&gt;txn_begin()</a> method.

  The <a href="db_hotbackup.md" class="xref" title="db_hotbackup">db_hotbackup</a> utility implements the protocol described above.

- `DB_DSYNC_DB`

  Configure Berkeley DB to flush database writes to the backing disk before returning from the write system call, rather than flushing database writes explicitly in a separate system call, as necessary. This is only available on some systems (for example, systems supporting the IEEE/ANSI Std 1003.1 (POSIX) standard O_DSYNC flag, or systems supporting the Windows FILE_FLAG_WRITE_THROUGH flag). This flag may result in inaccurate file modification times and other file-level information for Berkeley DB database files. This flag will almost certainly result in a performance decrease on most systems. This flag is only applicable to certain filesysystems (for example, the Veritas VxFS filesystem), where the filesystem's support for trickling writes back to stable storage behaves badly (or more likely, has been misconfigured).

  Calling `DB_ENV->set_flags()` with the DB_DSYNC_DB flag only affects the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle (and any other Berkeley DB handles opened within the scope of that handle). For consistent behavior across the environment, all <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handles opened in the environment must either set the DB_DSYNC_DB flag or the flag should be specified in the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> configuration file.

  The DB_DSYNC_DB flag may be used to configure Berkeley DB at any time during the life of the application.

- `DB_MPOOL_AIO`

  If set, the Berkeley DB memory pool performs buffer-pool writeback (checkpoint, sync, and trickle) asynchronously, using a per-process native asynchronous I/O engine (io_uring, IOCP, POSIX aio, or a portable thread-pool fallback) when one is available. When this flag is not set — the default — or when no asynchronous I/O backend is configured for the platform, writeback is performed synchronously exactly as in prior releases. Foreground buffer eviction is always synchronous.

  Asynchronous writeback can reduce stalls under write-heavy or cache-pressure workloads on storage that benefits from multiple outstanding I/Os; on a single slow device it may not help, so it is opt-in.

  Calling `DB_ENV->set_flags()` with the DB_MPOOL_AIO flag only affects the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle (and any other Berkeley DB handles opened within the scope of that handle). The asynchronous I/O context is private to each process.

  The DB_MPOOL_AIO flag may be used to configure Berkeley DB at any time during the life of the application.

- `DB_MULTIVERSION`

  If set, all databases in the environment will be opened as if DB_MULTIVERSION is passed to the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method. This flag will be ignored for queue databases for which DB_MULTIVERSION is not supported.

  Calling `DB_ENV->set_flags()` with the DB_MULTIVERSION flag only affects the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle (and any other Berkeley DB handles opened within the scope of that handle). For consistent behavior across the environment, all <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handles opened in the environment must either set the DB_MULTIVERSION flag or the flag should be specified in the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> configuration file.

  The DB_MULTIVERSION flag may be used to configure Berkeley DB at any time during the life of the application.

- `DB_NOLOCKING`

  If set, Berkeley DB will grant all requested mutual exclusion mutexes and database locks without regard for their actual availability. This functionality should never be used for purposes other than debugging.

  Calling `DB_ENV->set_flags()` with the DB_NOLOCKING flag only affects the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle (and any other Berkeley DB handles opened within the scope of that handle).

  The DB_NOLOCKING flag may be used to configure Berkeley DB at any time during the life of the application.

- `DB_NOMMAP`

  If set, Berkeley DB will copy read-only database files into the local cache instead of potentially mapping them into process memory (see the description of the <a href="envset_mp_mmapsize.md" class="xref" title="DB_ENV-&gt;set_mp_mmapsize()">DB_ENV-&gt;set_mp_mmapsize()</a> method for further information).

  Calling `DB_ENV->set_flags()` with the DB_NOMMAP flag only affects the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle (and any other Berkeley DB handles opened within the scope of that handle). For consistent behavior across the environment, all <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handles opened in the environment must either set the DB_NOMMAP flag or the flag should be specified in the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> configuration file.

  The DB_NOMMAP flag may be used to configure Berkeley DB at any time during the life of the application.

- `DB_NOPANIC`

  If set, Berkeley DB will ignore any panic state in the database environment. (Database environments in a panic state normally refuse all attempts to call Berkeley DB functions, returning <a href="../../guides/programmer_reference/program_errorret.md#program_errorret.DB_RUNRECOVERY" class="olink">DB_RUNRECOVERY</a>.) This functionality should never be used for purposes other than debugging.

  Calling `DB_ENV->set_flags()` with the DB_NOPANIC flag only affects the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle (and any other Berkeley DB handles opened within the scope of that handle).

  The DB_NOPANIC flag may be used to configure Berkeley DB at any time during the life of the application.

- `DB_OVERWRITE`

  Overwrite files stored in encrypted formats before deleting them. Berkeley DB overwrites files using alternating 0xff, 0x00 and 0xff byte patterns. For file overwriting to be effective, the underlying file must be stored on a fixed-block filesystem. Systems with journaling or logging filesystems will require operating system support and probably modification of the Berkeley DB sources.

  Calling `DB_ENV->set_flags()` with the DB_OVERWRITE flag only affects the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle (and any other Berkeley DB handles opened within the scope of that handle).

  The DB_OVERWRITE flag may be used to configure Berkeley DB at any time during the life of the application.

- `DB_PANIC_ENVIRONMENT`

  If set, Berkeley DB will set the panic state for the database environment. (Database environments in a panic state normally refuse all attempts to call Berkeley DB functions, returning <a href="../../guides/programmer_reference/program_errorret.md#program_errorret.DB_RUNRECOVERY" class="olink">DB_RUNRECOVERY</a>.) This flag may not be specified using the environment's <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file.

  Calling `DB_ENV->set_flags()` with the DB_PANIC_ENVIRONMENT flag affects the database environment, including all threads of control accessing the database environment.

  The DB_PANIC_ENVIRONMENT flag may be used to configure Berkeley DB only after the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called.

- `DB_REGION_INIT`

  In some applications, the expense of page-faulting the underlying shared memory regions can affect performance. (For example, if the page-fault occurs while holding a lock, other lock requests can convoy, and overall throughput may decrease.) If set, Berkeley DB will page-fault shared regions into memory when initially creating or joining a Berkeley DB environment. In addition, Berkeley DB will write the shared regions when creating an environment, forcing the underlying virtual memory and filesystems to instantiate both the necessary memory and the necessary disk space. This can also avoid out-of-disk space failures later on.

  Calling `DB_ENV->set_flags()` with the DB_REGION_INIT flag only affects the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle (and any other Berkeley DB handles opened within the scope of that handle). For consistent behavior across the environment, all <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handles opened in the environment must either set the DB_REGION_INIT flag or the flag should be specified in the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> configuration file.

  The DB_REGION_INIT flag may be used to configure Berkeley DB at any time during the life of the application.

- `DB_TIME_NOTGRANTED`

  If set, database calls timing out based on lock or transaction timeout values will return <a href="../../guides/programmer_reference/program_errorret.md#program_errorret.DB_LOCK_NOTGRANTED" class="olink">DB_LOCK_NOTGRANTED</a> instead of <a href="../../guides/programmer_reference/program_errorret.md#program_errorret.DB_LOCK_DEADLOCK" class="olink">DB_LOCK_DEADLOCK</a>. This allows applications to distinguish between operations which have deadlocked and operations which have exceeded their time limits.

  Calling `DB_ENV->set_flags()` with the DB_TIME_NOTGRANTED flag only affects the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle (and any other Berkeley DB handles opened within the scope of that handle). For consistent behavior across the environment, all <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handles opened in the environment must either set the DB_TIME_NOTGRANTED flag or the flag should be specified in the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> configuration file.

  The `DB_TIME_NOTGRANTED` flag may be used to configure Berkeley DB at any time during the life of the application.

  Note that the <a href="lockget.md" class="xref" title="DB_ENV-&gt;lock_get()">DB_ENV-&gt;lock_get()</a> and <a href="lockvec.md" class="xref" title="DB_ENV-&gt;lock_vec()">DB_ENV-&gt;lock_vec()</a> methods are unaffected by this flag.

- `DB_TXN_NOSYNC`

  If set, Berkeley DB will not write or synchronously flush the log on transaction commit. This means that transactions exhibit the ACI (atomicity, consistency, and isolation) properties, but not D (durability); that is, database integrity will be maintained, but if the application or system fails, it is possible some number of the most recently committed transactions may be undone during recovery. The number of transactions at risk is governed by how many log updates can fit into the log buffer, how often the operating system flushes dirty buffers to disk, and how often the log is checkpointed.

  Calling `DB_ENV->set_flags()` with the DB_TXN_NOSYNC flag only affects the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle (and any other Berkeley DB handles opened within the scope of that handle). For consistent behavior across the environment, all <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handles opened in the environment must either set the DB_TXN_NOSYNC flag or the flag should be specified in the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> configuration file.

  The DB_TXN_NOSYNC flag may be used to configure Berkeley DB at any time during the life of the application.

- `DB_TXN_NOWAIT`

  If set and a lock is unavailable for any Berkeley DB operation performed in the context of a transaction, cause the operation to return <a href="../../guides/programmer_reference/program_errorret.md#program_errorret.DB_LOCK_DEADLOCK" class="olink">DB_LOCK_DEADLOCK</a> (or <a href="../../guides/programmer_reference/program_errorret.md#program_errorret.DB_LOCK_NOTGRANTED" class="olink">DB_LOCK_NOTGRANTED</a> if configured using the DB_TIME_NOTGRANTED flag).

  Calling `DB_ENV->set_flags()` with the DB_TXN_NOWAIT flag only affects the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle (and any other Berkeley DB handles opened within the scope of that handle). For consistent behavior across the environment, all <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handles opened in the environment must either set the DB_TXN_NOWAIT flag or the flag should be specified in the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> configuration file.

  The DB_TXN_NOWAIT flag may be used to configure Berkeley DB at any time during the life of the application.

- `DB_TXN_SNAPSHOT`

  If set, all transactions in the environment will be started as if DB_TXN_SNAPSHOT were passed to the <a href="txnbegin.md" class="xref" title="DB_ENV-&gt;txn_begin()">DB_ENV-&gt;txn_begin()</a> method, and all non-transactional cursors will be opened as if DB_TXN_SNAPSHOT were passed to the <a href="dbcursor.md" class="xref" title="DB-&gt;cursor()">DB-&gt;cursor()</a> method.

  Calling `DB_ENV->set_flags()` with the DB_TXN_SNAPSHOT flag only affects the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle (and any other Berkeley DB handles opened within the scope of that handle). For consistent behavior across the environment, all <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handles opened in the environment must either set the DB_TXN_SNAPSHOT flag or the flag should be specified in the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> configuration file.

  The DB_TXN_SNAPSHOT flag may be used to configure Berkeley DB at any time during the life of the application.

- `DB_TXN_WRITE_NOSYNC`

  If set, Berkeley DB will write, but will not synchronously flush, the log on transaction commit. This means that transactions exhibit the ACI (atomicity, consistency, and isolation) properties, but not D (durability); that is, database integrity will be maintained, but if the system fails, it is possible some number of the most recently committed transactions may be undone during recovery. The number of transactions at risk is governed by how often the system flushes dirty buffers to disk and how often the log is checkpointed.

  Calling `DB_ENV->set_flags()` with the DB_TXN_WRITE_NOSYNC flag only affects the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle (and any other Berkeley DB handles opened within the scope of that handle). For consistent behavior across the environment, all <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handles opened in the environment must either set the DB_TXN_WRITE_NOSYNC flag or the flag should be specified in the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> configuration file.

  The DB_TXN_WRITE_NOSYNC flag may be used to configure Berkeley DB at any time during the life of the application.

- `DB_YIELDCPU`

  If set, Berkeley DB will yield the processor immediately after each page or mutex acquisition. This functionality should never be used for purposes other than stress testing.

  Calling `DB_ENV->set_flags()` with the DB_YIELDCPU flag only affects the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle (and any other Berkeley DB handles opened within the scope of that handle). For consistent behavior across the environment, all <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handles opened in the environment must either set the DB_YIELDCPU flag or the flag should be specified in the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> configuration file.

  The DB_YIELDCPU flag may be used to configure Berkeley DB at any time during the life of the application.

#### onoff

If the **onoff** parameter is zero, the specified flags are cleared; otherwise they are set.

### Errors

The `DB_ENV->set_flags()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
