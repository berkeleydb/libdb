---
title: "Determining the Scope of the Modifications"
api-name: "Determining the Scope of the Modifications"
source: docs/porting/modscope.html
---
## Determining the Scope of the Modifications

<span class="sect2"> [Do Changes Need to be Made to the Operating System Functionality?](modscope.md#osfunc) </span>

<span class="sect2"> [Are Some Standard Functions Missing on the Target Platform?](modscope.md#standardfunc) </span>

<span class="sect2"> [How Will the Port Handle Shared Memory?](modscope.md#sharedmem) </span>

<span class="sect2"> [What Type of Mutexes Will the Port Use?](modscope.md#typemutex) </span>

<span class="sect2"> [Do Any Other Changes Need to be Made?](modscope.md#otherchanges) </span>

Once you have a good build of Berkeley DB on a UNIX or UNIX-like system, look over the code to determine what type of code changes you need to make so that you can successfully build Berkeley DB on your target system. This process involves determining:

- <a href="modscope.md#osfunc" class="xref" title="Do Changes Need to be Made to the Operating System Functionality?">Do Changes Need to be Made to the Operating System Functionality?</a>

- <a href="modscope.md#standardfunc" class="xref" title="Are Some Standard Functions Missing on the Target Platform?">Are Some Standard Functions Missing on the Target Platform?</a>

- <a href="modscope.md#sharedmem" class="xref" title="How Will the Port Handle Shared Memory?">How Will the Port Handle Shared Memory?</a>

- <a href="modscope.md#typemutex" class="xref" title="What Type of Mutexes Will the Port Use?">What Type of Mutexes Will the Port Use?</a>

- <a href="modscope.md#otherchanges" class="xref" title="Do Any Other Changes Need to be Made?">Do Any Other Changes Need to be Made?</a>

### Do Changes Need to be Made to the Operating System Functionality?

Berkeley DB uses about forty operating system primitives. The Berkeley DB distribution contains files which are wrappers around these operating system primitives that act as an abstraction layer to separate the main Berkeley DB code from operating system and architecture-specific components. You <span class="emphasis">*must*</span> port these files (or versions of these files) whenever you port Berkeley DB to a new platform.

Within a Berkeley DB distribution, typically, there is only a single version of these files for all platforms that Berkeley DB supports. Those versions of the files live in the `os` directory of the distribution and follow the ANSI C and POSIX 1003.1 standards. Within each file, there is usually one, but sometimes several functions (for example, the `os_alloc.c` file contains functions such as `malloc`, `realloc`, `strdup`, `free`). The following table describes the files in the os directory of the Berkeley DB distribution along with the POSIX functions that must be ported.

<table data-border="1" width="80%">
<thead>
<tr>
<th><p>POSIX Functions</p></th>
<th><p>Internal Function Name</p></th>
<th><p>Source File</p></th>
</tr>
</thead>
<tbody>
<tr>
<td><p>abort() is required if diagnostic build is used or if snprintf is not provided by the platform</p></td>
<td><p>__os_abort()</p></td>
<td><p>os_abort.c</p></td>
</tr>
<tr>
<td><p>freeaddrinfo()</p>
<p>getaddrinfo(), htonl(), htons(), inet_addr(), and gethostbyname() are required for Replication Manager</p></td>
<td><p>__os_getaddrinfo(), __os_freeaddrinfo()</p></td>
<td>os_addrinfo.c</td>
</tr>
<tr>
<td><p>malloc(), realloc(), strdup(), free(), memcpy(), memset(), strlen()</p></td>
<td>__os_umalloc(), __os_urealloc(), __os_ufree(), __os_strdup(), __os_calloc(), __os_malloc(), __os_realloc(), __os_free(), __os_guard(), __ua_memcpy()</td>
<td><p>os_alloc.c</p></td>
</tr>
<tr>
<td><p>clock_gettime(), time(), gettimeofday()</p></td>
<td><p>__os_gettime()</p></td>
<td><p>os_clock.c</p></td>
</tr>
<tr>
<td><p>sysconf()</p></td>
<td><p>__os_cpu_count()</p></td>
<td><p>os_cpu.c</p></td>
</tr>
<tr>
<td><p>ctime(), ctime_r()</p></td>
<td><p>__os_ctime()</p></td>
<td><p>os_ctime.c</p></td>
</tr>
<tr>
<td><p>opendir(), closdir(), readdir(), stat()</p></td>
<td><p>__os_dirlist(), __os_dirfree()</p></td>
<td><p>os_dir.c</p></td>
</tr>
<tr>
<td><p>strncpy()</p></td>
<td><p>__os_get_errno_ret_zero(), __os_get_errno(), __os_get_syserr(), __os_set_errno(), __os_strerror(), __os_posix_err()</p></td>
<td><p>os_errno.c</p></td>
</tr>
<tr>
<td><p>fcntl() is required for DB_REGISTER</p></td>
<td><p>__os_fdlock()</p></td>
<td><p>os_flock.c</p></td>
</tr>
<tr>
<td><p>fsync(), fdatasync()</p></td>
<td><p>__vx_fsync(), __os_fsync()</p></td>
<td><p>os_fsync.c</p></td>
</tr>
<tr>
<td><p>getenv() and strcpy() are required when environment variables are used to configure the database</p></td>
<td><p>__os_getenv()</p></td>
<td><p>os_getenv.c</p></td>
</tr>
<tr>
<td><p>close(), open()</p></td>
<td><p>__os_openhandle(), __os_closehandle()</p></td>
<td><p>os_handle.c</p></td>
</tr>
<tr>
<td><p>getpid()</p>
<p>pthread_self() is required for replication and failchk functionality</p></td>
<td><p>__os_id()</p></td>
<td><p>os_pid.c</p></td>
</tr>
<tr>
<td><p>shmget(), shmdt(), shmctl(), and shmat() are required when envrionment uses share memory for regions</p>
<p>munmap() is required when envrionment uses memory mapped files for regions or read-only databases</p>
<p>munlock() is required when environment is configured with DB_LOCKDOWN</p></td>
<td><p>__os_attach(), __os_detach(), __os_mapfile(), __os_unmapfile(), __os_map(), __shm_mode(), __no_system_mem()</p></td>
<td><p>os_map.c</p></td>
</tr>
<tr>
<td><p>mkdir(), chmod()</p></td>
<td><p>__os_mkdir()</p></td>
<td><p>os_mkdir.c</p></td>
</tr>
<tr>
<td><p>fchmod()</p>
<p>directio() is required when explicitly enabling DIRECTIO_ON</p></td>
<td><p>__os_open()</p></td>
<td><p>os_open.c</p></td>
</tr>
<tr>
<td><p>rename()</p></td>
<td><p>__os_rename()</p></td>
<td><p>os_rename.c</p></td>
</tr>
<tr>
<td><p>getuid() is required when environment variables are used to configure the database</p></td>
<td><p>__os_isroot()</p></td>
<td><p>os_root.c</p></td>
</tr>
<tr>
<td><p>read(), write(), pread(), pwrite()</p></td>
<td><p>__os_io(), __os_read(), __os_write(), __os_physwrite()</p></td>
<td><p>os_rw.c</p></td>
</tr>
<tr>
<td><p>lseek()</p></td>
<td><p>__os_seek()</p></td>
<td><p>os_seek.c</p></td>
</tr>
<tr>
<td><p>stat(), fstat()</p></td>
<td><p>__os_exists(), __os_ioinfo()</p></td>
<td><p>os_stat.c</p></td>
</tr>
<tr>
<td><p>ftruncate() is required when using truncate</p></td>
<td><p>__os_truncate()</p></td>
<td><p>os_truncate.c</p></td>
</tr>
<tr>
<td><p>unlink()</p></td>
<td><p>__os_unlink()</p></td>
<td><p>os_unlink.c</p></td>
</tr>
<tr>
<td><p>yield(), sched_yield()</p></td>
<td><p>__os_yield(), __os_sleep()</p></td>
<td><p>os.yield.c</p></td>
</tr>
</tbody>
</table>

When the operating system primitives on the target platform are identical or close to the POSIX semantics that Berkeley DB requires, then no code changes or minimal code changes to the files in the `os` directory are required. If the operating system primitives are quite different, then some code changes may be required to bridge the gap between the requirements of Berkeley DB and what the operating system provides.

Where different code is required, you write an entirely different version of the file and place it in an `os`\_<span class="emphasis">*xxx*</span> directory where <span class="emphasis">*xxx*</span> represents a platform name. There are `os`\_<span class="emphasis">*xxx*</span> subdirectories in the Berkeley DB distribution for several established non-POSIX platforms. For example, there is a `os_vxworks` directory that contains VxWorks versions of some of the files in the os directory, and Windows versions of some files are in the `os_windows` directory. If your target platform needs a different version of a file, you will need to write that file and place it in a new `os`\_<span class="emphasis">*xxx*</span> directory that you create for your target platform.

### Are Some Standard Functions Missing on the Target Platform?

In some cases, the target platform may not provide the few POSIX functions required by Berkeley DB or the functions provided by the target platform may not operate in a standard compliant way. Berkeley DB provides replacement functions in the `clib` directory of the Berkeley DB distribution.

You need to determine how your target platfrom handles these functions:

- When the target platform does <span class="emphasis">*not*</span> have a POSIX function required by Berkeley DB, no action is required on your part. When Berekely DB cannot find one of these functions on the target platform, it automatically uses the replacement functions supplied in the `clib` directory of the Berkeley DB distribution. For example, if the target platform does not have the `atoi` or `strtol` functions, Berkeley DB uses `clib/atoi.c` and `clib/strtol.c`.

- When the target platform has a function required by Berekely DB, but that function operates in a non-standard compliant way, you can code to the replacement functions supplied in the `clib` directory.

### How Will the Port Handle Shared Memory?

In order to write multiprocess database applications (not multithreaded, but threads of control running in different address spaces), Berkeley DB must be able to name pieces of shared memory and access them from multiple processes.

On UNIX/POSIX systems, Berkeley DB uses `mmap` and `shmget` for that purpose, but any interface that provides access to named shared memory is sufficient. If you have a simple, flat address space, you should be able to use the code in `os_vxworks/os_map.c` as a starting point for the port.

If you are not intending to write multiprocess database applications, then this won't be necessary, as Berkeley DB can simply allocate memory from the heap if all threads of control will live in a single address space.

### What Type of Mutexes Will the Port Use?

Berkeley DB requires some form of self-blocking mutual exclusion mutex. Blocking mutexes are preferred as they tend to be less CPU-expensive and less likely to cause thrashing. If blocking mutexes are not available, however, test-and-set will work as well. The code for mutexes is in two places in the system: the include file `dbinc/mutex_int.h`, and the distribution directory `mutex`.

### Do Any Other Changes Need to be Made?

In most cases, you do not need to make any changes to the Berkeley DB source code that is not in the abstraction layer (that is, in the `os` directory) as that code is designed to be platform-independent code. However, in some situations, the compiler for the target platform is non-standard and may raise errors when compiling some aspects of the Berkeley DB code (for example, additional casting may be required, or a certain type may cause a problem). In these cases, you will need to modify the generic Berkeley DB code in order to have error-free compilation.
