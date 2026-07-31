---
title: "4.0.14 Change Log"
api-name: "4.0.14 Change Log"
source: docs/upgrading/changelog_4_0_14.html
---
## 4.0.14 Change Log

<span class="sect2"> [Major New Features:](changelog_4_0_14.md#idp51113768) </span>

<span class="sect2"> [General Environment Changes:](changelog_4_0_14.md#idp51101344) </span>

<span class="sect2"> [General Access Method Changes:](changelog_4_0_14.md#idp51103296) </span>

<span class="sect2"> [Btree Access Method Changes:](changelog_4_0_14.md#idp51105152) </span>

<span class="sect2"> [Hash Access Method Changes:](changelog_4_0_14.md#idp51109416) </span>

<span class="sect2"> [Queue Access Method Changes:](changelog_4_0_14.md#idp51112664) </span>

<span class="sect2"> [Recno Access Method Changes:](changelog_4_0_14.md#idp51113832) </span>

<span class="sect2"> [C++ API Changes:](changelog_4_0_14.md#idp51115760) </span>

<span class="sect2"> [Java API Changes:](changelog_4_0_14.md#idp51126328) </span>

<span class="sect2"> [Tcl API Changes:](changelog_4_0_14.md#idp51116840) </span>

<span class="sect2"> [RPC Client/Server Changes:](changelog_4_0_14.md#idp51117920) </span>

<span class="sect2"> [XA Resource Manager Changes:](changelog_4_0_14.md#idp51118608) </span>

<span class="sect2"> [Locking Subsystem Changes:](changelog_4_0_14.md#idp51118928) </span>

<span class="sect2"> [Logging Subsystem Changes:](changelog_4_0_14.md#idp51103680) </span>

<span class="sect2"> [Memory Pool Subsystem Changes:](changelog_4_0_14.md#idp51122816) </span>

<span class="sect2"> [Transaction Subsystem Changes:](changelog_4_0_14.md#idp51109800) </span>

<span class="sect2"> [Utility Changes:](changelog_4_0_14.md#idp51113048) </span>

<span class="sect2"> [Database or Log File On-Disk Format Changes:](changelog_4_0_14.md#idp51125248) </span>

<span class="sect2"> [Configuration, Documentation, Portability and Build Changes:](changelog_4_0_14.md#idp51126712) </span>

### Major New Features:

1.  Group commit. \[#42\]
2.  Single-master replication. \[#44\]
3.  Support for VxWorks AE; Vxworks support certified by WindRiver Systems Inc. \[#4401\]

### General Environment Changes:

1.  The db_env_set_pageyield interface has been replaced by a new flag (DB_YIELDCPU) for the DB_ENV-\>set_flags interface.
2.  The db_env_set_panicstate interface has been replaced by a new flag (DB_PANIC_STATE) for the DB_ENV-\>set_flags interface.
3.  The db_env_set_region_init interface has been replaced by a new flag (DB_REGION_INIT) for the DB_ENV-\>set_flags interface.
4.  The db_env_set_tas_spins interface has been replaced by the DB_ENV-\>set_tas_spins method.
5.  The DB_ENV-\>set_mutexlocks interface has been replaced by a new flag (DB_NOLOCKING) for the DB_ENV-\>set_flags interface.
6.  Fix a bug where input values from the DB_CONFIG file could overflow.
7.  The C API lock, log, memory pool and transaction interfaces have been converted to method based interfaces; see the Upgrade documentation for specific details. \[#920\]
8.  Fix a bug in which some DB_ENV configuration information could be lost by a failed DB_ENV-\>open command. \[#4608\]
9.  Fix a bug where Berkeley DB could fail if the application attempted to allocate new database pages while the system was unable to write new log file buffers. \[#4928\]

### General Access Method Changes:

1.  Add a new flag (DB_GET_BOTH_RANGE) that adds support for range searches within sorted duplicate data sets. \[#3378\]
2.  Fix a bug in which the DB-\>get or DB-\>pget methods, when used with secondary indices, could incorrectly leave an internally-created database cursor open. \[#4465\]
3.  The DB-\>set_alloc method can no longer be called when the database is part of a database environment. \[#4599\]

### Btree Access Method Changes:

1.  Fix a bug where a lock could be leaked when a thread calling DB-\>stat on a Btree database was selected to resolve a deadlock. \[#4509\]

### Hash Access Method Changes:

1.  Fix a bug where bulk return using the MULTIPLE_KEY flag on a Hash database would only return entries from a single bucket. \[#4313\]

### Queue Access Method Changes:

1.  Delete extent files whenever the leading record is deleted, instead of only when a DB_CONSUME operation was performed. \[#4307\]

### Recno Access Method Changes:

1.  Fix a bug where the delete of a record in a Recno database could leak a lock in non-transactional applications. \[#4351\]
2.  Fix a bug where the DB_THREAD flag combined with a backing source file could cause an infinite loop. \[#4581\]

### C++ API Changes:

> None.

### Java API Changes:

1.  Added implementation of DbEnv.lock_vec for Java. \[#4094\] Added some minimal protection so that the same Java Dbt cannot be used twice in the same API call, this will often catch multithreading programming errors with Dbts. \[#4094\]
2.  Fix a bug in which a Db.put call with the Db.DB_APPEND would fail to correctly return the newly put record's record number. \[#4527\]
3.  Fixed problems occurring in multithreaded java apps that use callbacks. \[#4467\]

### Tcl API Changes:

1.  Fix a bug in which large integers could be handled incorrectly by the Tcl interface on 64-bit machines. \[#4371\]

### RPC Client/Server Changes:

1.  The DB_ENV-\>set_server interface has been removed.

### XA Resource Manager Changes:

> None.

### Locking Subsystem Changes:

1.  The C++ (Java) API DbLock::put (DbLock.put) method has been changed to be a method off the DbEnv handle rather than the DbLock handle.
2.  Locker IDs may now wrap-around. \[#864\]
3.  Explicitly allocated locker IDs must now be freed. \[#864\]
4.  Add per-environment, per-lock and per-transaction interfaces to support timeout based lock requests and "deadlock" detection. \[#1855\]
5.  Add support for interrupting a waiting locker. \[#1976\]
6.  Implemented DbEnv.lock_vec for Java. \[#4094\]

### Logging Subsystem Changes:

1.  Fix a bug where the size of a log file could not be set to the default value. \[#4567\]
2.  Fix a bug where specifying a non-default log file size could cause other processes to be unable to join the environment and read its log files. \[#4567\]
3.  Fix a bug where Berkeley DB could keep open file descriptors to log files returned by the DB_ENV-\>log_archive method (or the db_archive utility), making it impossible to move or remove them on Windows systems. \[#3969\]
4.  Replace the log_get interface with a cursor into the log file. \[#0043\]

### Memory Pool Subsystem Changes:

1.  Add the DB_ODDFILESIZE flag to the DB_MPOOLFILE-\>open method supporting files not a multiple of the underlying page size in length.
2.  Convert memp_XXX functional interfaces to a set of methods, either base methods off the DB_ENV handle or methods off of a DB_MPOOLFILE handle. \[#920\]
3.  Add the DB_ODDFILESIZE flag to the DB_MPOOLFILE-\>open method supporting files not a multiple of the underlying page size in length.
4.  Fix a bug where threads of control could deadlock opening a database environment with multiple memory pool caches. \[#4696\]
5.  Fix a bug where the space needed for per-file memory pool statistics was incorrectly calculated. \[#4772\]

### Transaction Subsystem Changes:

1.  Transaction IDs may now wrap-around. \[#864\]
2.  Release read locks before performing logging operations at commit. \[#4219\]

### Utility Changes:

1.  Fix a bug in which the db_dump utility would incorrectly attach to transaction, locking, or logging regions when salvaging, and thus could not be used to salvage databases in environments where these regions were present. \[#4305\]
2.  Fix a bug in which the DB salvager could produce incorrectly formatted output for certain classes of corrupt database. \[#4305\]
3.  Fix a bug in which the DB salvager could incorrectly salvage files containing multiple databases. \[#4305\]
4.  Fix a bug where unprintable characters in subdatabase names could cause a dump of a database that could not then be loaded. \[#4688\]
5.  Increase the size of the cache created by the db_stat and db_verify utilities to avoid failure on large databases. \[#4688\] \[#4787\]
6.  Fix a bug in which a database verification performed with the DB_ORDERCHKONLY flag could fail incorrectly. \[#4757\]
7.  Fix a bug which caused db_stat to display incorrect information about GB size caches. \[#4812\]

### Database or Log File On-Disk Format Changes:

1.  The on-disk log format changed.

### Configuration, Documentation, Portability and Build Changes:

1.  Fix a bug where Win9X systems region names could collide.
2.  Fix a bug where configuring Berkeley DB to build the C++ API without also configuring for a shared library build would fail to build the C++ library. \[#4343\]
3.  Change Berkeley DB installation to not strip binaries if --enable-debug was specified as a configuration option. \[#4318\]
4.  Add the -pthread flag to AIX, FreeBSD and OSF/1 library loads. \[#4350\]
5.  Fix a bug where the Berkeley DB 1.85 compatibility API failed to load in the 3.3.11 release. \[#4368\]
6.  Port the Berkeley DB utility programs to the VxWorks environment. \[#4378\]
7.  Made change to configuration so that dynamic libraries link correctly when C++ is used on AIX. \[#4381\]
8.  Fix a variety of problems that prevented the Berkeley DB source tree from building on systems without ANSI C compiler support (for example, SunOS 4.X). \[#4398\]
9.  Added missing DbMultiple\*Iterator Java files to Makefile.in. \[#4404\]
10. Fix a bug that could prevent the db_dump185 utility from dumping Berkeley DB version 1.86 hash databases. \[#4418\]
11. Reduce the number of calls setting the errno value, to improve performance on Windows/NT in MT environments. \[#4432\]
12. Fix for Darwin (and probably some other) OS's that were getting 'yes' or other garbage in generated makefiles in place of a shared library name. \[#4453\]
13. C++: Remove inlining for constructor of tmpString internal class. This fixes warnings on Solaris profiling builds. \[#4473\]
14. DB now restarts system calls that are interrupted by signals. \[#4480\]
15. Fixed warnings for compiling Java native code on Solaris and OSF/1. \[#4571\]
16. Added better configuration for Java on Tru64 (OSF/1), Solaris,
17. Java files are now built as jar files. Berkeley DB classes are put into db.jar (which is an installed file on UNIX) and examples are put into dbexamples.jar. The classes directory is now a subdirectory of the build directory, rather than in java/classes. \[#4575\]
18. Support Cygwin installation process. \[#4611\]
19. Correct the Java secondary_key_create method signature. \[#4777\]
20. Export additional Berkeley DB interfaces on Windows to support application-specific logging and recovery. \[#4827\]
21. Always complain when using version 2.96 of the gcc compiler. \[#4878\]
22. Add compile and load-time flags to configure for threads on UnixWare and OpenUNIX. \[#4552\] \[#4950\]
