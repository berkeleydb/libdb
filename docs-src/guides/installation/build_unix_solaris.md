---
title: "Solaris"
api-name: "Solaris"
source: docs/installation/build_unix_solaris.html
---
## Solaris

1.  **I can't compile and run multithreaded applications.**

    Special compile-time flags and additional libraries are required when compiling threaded applications on Solaris. If you are compiling a threaded application, you must compile with the D_REENTRANT flag and link with the libpthread.a or libthread.a libraries:

    ``` c
    cc -mt ...
    cc -D_REENTRANT ... -lthread
    cc -D_REENTRANT ... -lpthread
    ```

    The Berkeley DB library will automatically build with the correct options.

2.  **I've installed gcc on my Solaris system, but configuration fails because the compiler doesn't work.**

    On some versions of Solaris, there is a cc executable in the user's path, but all it does is display an error message and fail:

    ``` c
    % which cc
    /usr/ucb/cc
    % cc
    /usr/ucb/cc: language optional software package not installed
    ```

    Because Berkeley DB always uses the native compiler in preference to gcc, this is a fatal error. If the error message you are seeing is the following, then this may be the problem:

    ``` c
    checking whether the C compiler (cc -O) works... no
    configure: error: installation or configuration problem: C compiler 
    cannot create executables.
    ```

    The simplest workaround is to set your CC environment variable to the system compiler and reconfigure; for example:

    ``` c
    env CC=gcc ../dist/configure
    ```

    If you are using the --configure-cxx option, you may also want to specify a C++ compiler, for example the following:

    ``` c
    env CC=gcc CCC=g++ ../dist/configure
    ```

3.  **I see the error "libc internal error: \_rmutex_unlock: rmutex not held", followed by a core dump when running threaded or JAVA programs.**

    This is a known bug in Solaris 2.5 and it is fixed by Sun patch 103187-25.

4.  **I see error reports of nonexistent files, corrupted metadata pages and core dumps.**

    Solaris 7 contains a bug in the threading libraries (-lpthread, -lthread), which causes the wrong version of the pwrite routine to be linked into the application if the thread library is linked in after the C library. The result will be that the pwrite function is called rather than the pwrite64. To work around the problem, use an explicit link order when creating your application.

    Sun Microsystems is tracking this problem with Bug Id's 4291109 and 4267207, and patch 106980-09 to Solaris 7 fixes the problem:

    ``` c
    Bug Id: 4291109
    Duplicate of: 4267207
    Category: library
    Subcategory: libthread
    State: closed
    Synopsis: pwrite64 mapped to pwrite
    Description:
    When libthread is linked after libc, there is a table of functions in
    libthread that gets "wired into" libc via _libc_threads_interface().
    The table in libthread is wrong in both Solaris 7 and on28_35 for the
    TI_PWRITE64 row (see near the end).
    ```

5.  **I see corrupted databases when doing hot backups or creating a hot failover archive.**

    The Solaris cp utility is implemented using the mmap system call, and so writes are not blocked when it reads database pages. See <a href="../../guides/programmer_reference/transapp_reclimit.md" class="olink">Berkeley DB recoverability</a> for more information.

6.  **Performance is slow and the application is doing a lot of I/O to the disk on which the database environment's files are stored.**

    By default, Solaris periodically flushes dirty blocks from memory-mapped files to the backing filesystem. This includes the Berkeley DB database environment's shared memory regions and can affect Berkeley DB performance. Workarounds include creating the shared regions in system shared memory (<a href="../../api/c/envopen.md#envopen_DB_SYSTEM_MEM" class="olink">DB_SYSTEM_MEM</a>) or application private memory (<a href="../../api/c/envopen.md#envopen_DB_PRIVATE" class="olink">DB_PRIVATE</a>), or configuring Solaris to not flush memory-mapped pages. For more information, see the "Solaris Tunable Parameters Reference Manual: fsflush and Related Tunables".

7.  **I see errors about "open64" when building Berkeley DB applications.**

    System include files (most commonly fcntl.h) in some releases of AIX and Solaris redefine "open" when large-file support is enabled for applications. This causes problems when compiling applications because "open" is a method in the Berkeley DB APIs. To work around this problem:

    1.  Avoid including the problematical system include files in source code files which also include Berkeley DB include files and call into the Berkeley DB API.
    2.  Before building Berkeley DB, modify the generated include file db.h to itself include the problematical system include files.
    3.  Turn off Berkeley DB large-file support by specifying the <a href="build_unix_conf.md#build_unix_conf.--disable-largefile" class="link">--disable-largefile</a> configuration option and rebuilding.
