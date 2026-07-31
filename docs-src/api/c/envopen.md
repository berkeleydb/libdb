---
title: "DB_ENV->open()"
api-name: "DB_ENV->open()"
source: docs/api_reference/C/envopen.html
---
## DB_ENV-\>open()

``` c
#include <db.h>

int
DB_ENV->open(DB_ENV *dbenv, char *db_home, u_int32_t flags, int mode);  
```

The `DB_ENV->open()` method opens a Berkeley DB environment. It provides a structure for creating a consistent environment for processes using one or more of the features of Berkeley DB.

The `DB_ENV->open()` method method returns a non-zero error value on failure and 0 on success. If `DB_ENV->open()` fails, the <a href="envclose.md" class="xref" title="DB_ENV-&gt;close()">DB_ENV-&gt;close()</a> method must be called to discard the <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle.

### Warning

Using environments with some journaling filesystems might result in log file corruption. This can occur if the operating system experiences an unclean shutdown when a log file is being created. Please see <a href="../../programmer_reference/transapp_journal.html" class="olink">Using Recovery on Journaling Filesystems</a> in the *Berkeley DB Programmer's Reference Guide* for more information.

### Parameters

#### db_home

The **db_home** parameter is the database environment's home directory. For more information on **db_home**, and filename resolution in general, see <a href="../../programmer_reference/env_naming.html" class="olink">Berkeley DB File Naming</a>. The environment variable **DB_HOME** may be used as the path of the database home, as described in <a href="../../programmer_reference/env_naming.html" class="olink">Berkeley DB File Naming</a>.

When using a Unicode build on Windows (the default), the **db_home** argument will be interpreted as a UTF-8 string, which is equivalent to ASCII for Latin characters.

#### flags

The **flags** parameter specifies the subsystems that are initialized and how the application's environment affects Berkeley DB file naming, among other things. The **flags** parameter must be set to 0 or by bitwise inclusively **OR**'ing together one or more of the values described in this section.

Because there are a large number of flags that can be specified, they have been grouped together by functionality. The first group of flags indicates which of the Berkeley DB subsystems should be initialized.

The choice of subsystems initialized for a Berkeley DB database environment is specified by the thread of control initially creating the environment. Any subsequent thread of control joining the environment will automatically be configured to use the same subsystems as were created in the environment (unless the thread of control requests a subsystem not available in the environment, which will fail). Applications joining an environment, able to adapt to whatever subsystems have been configured in the environment, should open the environment without specifying any subsystem flags. Applications joining an environment, requiring specific subsystems from their environments, should open the environment specifying those specific subsystem flags.

- `DB_INIT_CDB`

  Initialize locking for the <a href="../../programmer_reference/cam.html#cam_intro" class="olink">Berkeley DB Concurrent Data Store</a> product. In this mode, Berkeley DB provides multiple reader/single writer access. The only other subsystem that should be specified with the `DB_INIT_CDB` flag is `DB_INIT_MPOOL`.

- `DB_INIT_LOCK`

  Initialize the locking subsystem. This subsystem should be used when multiple processes or threads are going to be reading and writing a Berkeley DB database, so that they do not interfere with each other. If all threads are accessing the database(s) read-only, locking is unnecessary. When the DB_INIT_LOCK flag is specified, it is usually necessary to run a deadlock detector, as well. See <a href="db_deadlock.md" class="link" title="db_deadlock">db_deadlock</a> and <a href="lockdetect.md" class="xref" title="DB_ENV-&gt;lock_detect()">DB_ENV-&gt;lock_detect()</a> for more information.

- `DB_INIT_LOG`

  Initialize the logging subsystem. This subsystem should be used when recovery from application or system failure is necessary. If the log region is being created and log files are already present, the log files are reviewed; subsequent log writes are appended to the end of the log, rather than overwriting current log entries.

- `DB_INIT_MPOOL`

  Initialize the shared memory buffer pool subsystem. This subsystem should be used whenever an application is using any Berkeley DB access method.

- `DB_INIT_REP`

  Initialize the replication subsystem. This subsystem should be used whenever an application plans on using replication. The `DB_INIT_REP` flag requires the `DB_INIT_TXN` and `DB_INIT_LOCK` flags also be configured.

  You can also specify this flag in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> configuration file. The syntax is a single line with the string "set_open_flags", one or more whitespace characters, the string "DB_INIT_REP", optionally one or more whitespace characters and the string "on" or "off". If the optional string is omitted, the default is "on"; for example, "set_open_flags DB_INIT_REP" or "set_open_flags DB_INIT_REP on". Because the DB_CONFIG file is read when the database environment is opened, it will silently overrule configuration done before that time.

- `DB_INIT_TXN`

  Initialize the transaction subsystem. This subsystem should be used when recovery and atomicity of multiple operations are important. The `DB_INIT_TXN` flag implies the `DB_INIT_LOG` flag.

The second group of flags govern what recovery, if any, is performed when the environment is initialized:

- `DB_RECOVER`

  Run normal recovery on this environment before opening it for normal use. If this flag is set, the `DB_CREATE` and `DB_INIT_TXN` flags must also be set, because the regions will be removed and re-created, and transactions are required for application recovery.

- `DB_RECOVER_FATAL`

  Run catastrophic recovery on this environment before opening it for normal use. If this flag is set, the `DB_CREATE` and `DB_INIT_TXN` flags must also be set, because the regions will be removed and re-created, and transactions are required for application recovery.

A standard part of the recovery process is to remove the existing Berkeley DB environment and create a new one in which to perform recovery. If the thread of control performing recovery does not specify the correct region initialization information (for example, the correct memory pool cache size), the result can be an application running in an environment with incorrect cache and other subsystem sizes. For this reason, the thread of control performing recovery should specify correct configuration information before calling the `DB_ENV->open()` method; or it should remove the environment after recovery is completed, leaving creation of the correctly sized environment to a subsequent call to the `DB_ENV->open()` method.

All Berkeley DB recovery processing must be single-threaded; that is, only a single thread of control may perform recovery or access a Berkeley DB environment while recovery is being performed. Because it is not an error to specify `DB_RECOVER` for an environment for which no recovery is required, it is reasonable programming practice for the thread of control responsible for performing recovery and creating the environment to always specify the `DB_CREATE` and `DB_RECOVER` flags during startup.

The third group of flags govern file-naming extensions in the environment:

- `DB_USE_ENVIRON`

  The Berkeley DB process' environment may be permitted to specify information to be used when naming files; see <a href="../../programmer_reference/env_naming.html" class="olink">Berkeley DB File Naming</a>. Because permitting users to specify which files are used can create security problems, environment information will be used in file naming for all users only if the `DB_USE_ENVIRON` flag is set.

- `DB_USE_ENVIRON_ROOT`

  The Berkeley DB process' environment may be permitted to specify information to be used when naming files; see <a href="../../programmer_reference/env_naming.html" class="olink">Berkeley DB File Naming</a>. Because permitting users to specify which files are used can create security problems, if the `DB_USE_ENVIRON_ROOT` flag is set, environment information will be used in file naming only for users with appropriate permissions (for example, users with a user-ID of 0 on `UNIX` systems).

Finally, there are a few additional unrelated flags:

- `DB_CREATE`

  Cause Berkeley DB subsystems to create any underlying files, as necessary.

- `DB_LOCKDOWN`

  Lock shared Berkeley DB environment files and memory-mapped databases into memory.

- `DB_FAILCHK`

  Internally call the <a href="envfailchk.md" class="xref" title="DB_ENV-&gt;failchk()">DB_ENV-&gt;failchk()</a> method as part of opening the environment. When `DB_FAILCHK` is specified, a check is made to ensure all `DB_ENV->failchk()` prerequisites are meet.

  If the `DB_FAILCHK` flag is used in conjunction with the `DB_REGISTER` flag, then a check will be made to see if the environment needs recovery. If recovery is needed, a call will be made to the `DB_ENV->failchk()` method to release any database reads locks held by the thread of control that exited and, if needed, to abort the unresolved transaction. If `DB_ENV->failchk()` determines environment recovery is still required, the recovery actions for `DB_REGISTER` will be followed.

  If the `DB_FAILCHK` flag is not used in conjunction with the `DB_REGISTER` flag, then make an internal call to `DB_ENV->failchk()` as the last step of opening the environment. If `DB_ENV->failchk()` determines database environment recovery is required, <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_RUNRECOVERY" class="olink">DB_RUNRECOVERY</a> will be returned.

- `DB_PRIVATE`

  Allocate region memory from the heap instead of from memory backed by the filesystem or system shared memory.

  ### Note

  Use of this flag means that the environment can only be accessed by one environment handle. The environment cannot be accessed by multiple processes. This is true even if one of those processes is one of the the Berkeley DB utilities. (For example, <a href="db_archive.md" class="link" title="db_archive">db_archive</a>, <a href="db_checkpoint.md" class="link" title="db_checkpoint">db_checkpoint</a> or <a href="db_stat.md" class="link" title="db_stat">db_stat</a>.) Nor can a single process open multiple handles to the environment.

  This flag has two effects on the Berkeley DB environment. First, all underlying data structures are allocated from per-process memory instead of from shared memory that is accessible to more than a single process. Second, mutexes are only configured to work between threads.

  See <a href="../../programmer_reference/env_region.html" class="olink">Shared Memory Regions</a> for more information.

  You can also specify this flag in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> configuration file. The syntax is a single line with the string "set_open_flags", one or more whitespace characters, the string "DB_PRIVATE", optionally one or more whitespace characters and the string "on" or "off". If the optional string is omitted, the default is "on"; for example, "set_open_flags DB_PRIVATE" or "set_open_flags DB_PRIVATE on". Because the DB_CONFIG file is read when the database environment is opened, it will silently overrule configuration done before that time.

- `DB_REGISTER`

  Check to see if recovery needs to be performed before opening the database environment. (For this check to be accurate, all processes using the environment must specify `DB_REGISTER` when opening the environment.) If recovery needs to be performed for any reason (including the initial use of the `DB_REGISTER` flag), and `DB_RECOVER` is also specified, recovery will be performed and the open will proceed normally. If recovery needs to be performed and `DB_RECOVER` is not specified, <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_RUNRECOVERY" class="olink">DB_RUNRECOVERY</a> will be returned. If recovery does not need to be performed, the `DB_RECOVER` flag will be ignored. See <a href="../../programmer_reference/transapp_app.html" class="olink">Architecting Transactional Data Store applications</a> for more information.

- `DB_SYSTEM_MEM`

  Allocate region memory from system shared memory instead of from heap memory or memory backed by the filesystem.

  See <a href="../../programmer_reference/env_region.html" class="olink">Shared Memory Regions</a> for more information.

- `DB_THREAD`

  Cause the <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle returned by `DB_ENV->open()` to be <span class="emphasis">*free-threaded;*</span> that is, concurrently usable by multiple threads in the address space. The `DB_THREAD ` flag should be specified if the <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle will be concurrently used by more than one thread in the process, or if any <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handles opened in the scope of the <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle will be concurrently used by more than one thread in the process.

  This flag is required when using the Replication Manager.

  You can also specify this flag in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> configuration file. The syntax is a single line with the string "set_open_flags", one or more whitespace characters, the string "DB_THREAD", optionally one or more whitespace characters and the string "on" or "off". If the optional string is omitted, the default is "on"; for example, "set_open_flags DB_THREAD" or "set_open_flags DB_THREAD on". Because the DB_CONFIG file is read when the database environment is opened, it will silently overrule configuration done before that time.

#### mode

On Windows systems, the mode parameter is ignored.

On UNIX systems or in IEEE/ANSI Std 1003.1 (POSIX) environments, files created by Berkeley DB are created with mode **mode** (as described in **chmod**(2)) and modified by the process' umask value at the time of creation (see **umask**(2)). Created files are owned by the process owner; the group ownership of created files is based on the system and directory defaults, and is not further specified by Berkeley DB. System shared memory segments created by Berkeley DB are created with mode **mode**, unmodified by the process' umask value. If **mode** is 0, Berkeley DB will use a default mode of readable and writable by both owner and group.

### Errors

The `DB_ENV->open()` method may fail and return one of the following non-zero errors:

#### DB_RUNRECOVERY

Either the `DB_REGISTER` flag was specified, a failure occurred, and no recovery flag was specified, or the `DB_FAILCHK` flag was specified and recovery was deemed necessary.

#### DB_VERSION_MISMATCH

The version of the Berkeley DB library doesn't match the version that created the database environment.

#### EAGAIN

The shared memory region was locked and (repeatedly) unavailable.

#### EINVAL

If the `DB_THREAD` flag was specified and fast mutexes are not available for this architecture; The `DB_HOME` or `TMPDIR` environment variables were set, but empty; An incorrectly formatted **NAME VALUE** entry or line was found; or if an invalid flag value or parameter was specified.

#### ENOENT

The file or directory does not exist.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
