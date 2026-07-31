---
title: "Shared memory regions"
api-name: "Shared memory regions"
source: docs/programmer_reference/env_region.html
---
## Shared memory regions

Each of the Berkeley DB subsystems within an environment is described by one or more regions, or chunks of memory. The regions contain all of the per-process and per-thread shared information (including mutexes), that comprise a Berkeley DB environment. These regions are created in one of three types of memory, depending on the flags specified to the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> method:

1.  If the <a href="../../api/c/envopen.md#envopen_DB_PRIVATE" class="olink">DB_PRIVATE</a> flag is specified to the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> method, regions are created in per-process heap memory; that is, memory returned by `malloc`(3).

    If this flag is specified, then you cannot open more than a single handle for the environment. For example, if both a server application and Berkeley DB utilities (for example, the <a href="../../api/c/db_archive.md" class="olink">db_archive</a> utility, the <a href="../../api/c/db_checkpoint.md" class="olink">db_checkpoint</a> utility or the <a href="../../api/c/db_stat.md" class="olink">db_stat</a> utility) are expected to access the environment, the <a href="../../api/c/envopen.md#envopen_DB_PRIVATE" class="olink">DB_PRIVATE</a> flag should not be specified because the second attempt to open the environment will fail.

2.  If the <a href="../../api/c/envopen.md#envopen_DB_SYSTEM_MEM" class="olink">DB_SYSTEM_MEM</a> flag is specified to <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a>, shared regions are created in system memory rather than files. This is an alternative mechanism for sharing the Berkeley DB environment among multiple processes and multiple threads within processes.

    The system memory used by Berkeley DB is potentially useful past the lifetime of any particular process. Therefore, additional cleanup may be necessary after an application fails because there may be no way for Berkeley DB to ensure that system resources backing the shared memory regions are returned to the system.

    The system memory that is used is architecture-dependent. For example, on systems supporting X/Open-style shared memory interfaces, such as UNIX systems, the `shmget`(2) and related System V IPC interfaces are used. Additionally, VxWorks systems use system memory. In these cases, an initial segment ID must be specified by the application to ensure that applications do not overwrite each other's database environments, so that the number of segments created does not grow without bounds. See the <a href="../../api/c/envset_shm_key.md" class="olink">DB_ENV-&gt;set_shm_key()</a> method for more information.

    On Windows platforms, the use of the <a href="../../api/c/envopen.md#envopen_DB_SYSTEM_MEM" class="olink">DB_SYSTEM_MEM</a> flag is problematic because the operating system uses reference counting to clean up shared objects in the paging file automatically. In addition, the default access permissions for shared objects are different from files, which may cause problems when an environment is accessed by multiple processes running as different users. See the <a href="../../guides/installation/build_win_notes.md" class="olink">Windows Notes</a> section in the Berkeley DB Installation and Build Guide for more information.

3.  If no memory-related flags are specified to <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a>, memory backed by the filesystem is used to store the regions. On UNIX systems, the Berkeley DB library will use the POSIX mmap interface. If mmap is not available, the UNIX shmget interfaces may be used instead, if they are available.

Any files created in the filesystem to back the regions are created in the environment home directory specified to the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> call. These files are named \_\_db.### (for example, \_\_db.001, \_\_db.002 and so on). When region files are backed by the filesystem, one file per region is created. When region files are backed by system memory, a single file will still be created because there must be a well-known name in the filesystem so that multiple processes can locate the system shared memory that is being used by the environment.

Statistics about the shared memory regions in the environment can be displayed using the **-e** option to the <a href="../../api/c/db_stat.md" class="olink">db_stat</a> utility.
