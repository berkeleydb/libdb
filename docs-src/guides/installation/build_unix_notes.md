---
title: "Architecture independent FAQ"
api-name: "Architecture independent FAQ"
source: docs/installation/build_unix_notes.html
---
## Architecture independent FAQ

1.  **I have gcc installed, but configure fails to find it.**

    Berkeley DB defaults to using the native C compiler if none is specified. That is usually "cc", but some platforms require a different compiler to build multithreaded code. To configure Berkeley DB to build with gcc, run configure as follows:

    ``` c
    env CC=gcc ../dist/configure ...
    ```

2.  **When compiling with gcc, I get unreferenced symbols; for example the following:**

    ``` c
    symbol __muldi3: referenced symbol not found
    symbol __cmpdi2: referenced symbol not found
    ```

    Berkeley DB often uses 64-bit integral types on systems supporting large files, and gcc performs operations on those types by calling library functions. These unreferenced symbol errors are usually caused by linking an application by calling "ld" rather than by calling "gcc": gcc will link in libgcc.a and will resolve the symbols. If that does not help, another possible workaround is to reconfigure Berkeley DB using the <a href="build_unix_conf.md#build_unix_conf.--disable-largefile" class="link">--disable-largefile</a> configuration option and then rebuild.

3.  **My C++ program traps during a failure in a DB call on my gcc-based system.**

    We believe there are some severe bugs in the implementation of exceptions for some gcc compilers. Exceptions require some interaction between compiler, assembler, and runtime libraries. We're not sure exactly what is at fault, but one failing combination is gcc 2.7.2.3 running on SuSE Linux 6.0. The problem on this system can be seen with a rather simple test case of an exception thrown from a shared library and caught in the main program.

    A variation of this problem seems to occur on AIX, although we believe it does not necessarily involve shared libraries on that platform.

    If you see a trap that occurs when an exception might be thrown by the Berkeley DB runtime, we suggest that you use static libraries instead of shared libraries. See the documentation for configuration. If this doesn't work and you have a choice of compilers, try using a more recent gcc- or a non-gcc based compiler to build Berkeley DB.

    Finally, you can disable the use of exceptions in the C++ runtime for Berkeley DB by using the <a href="../api_reference/CXX/envcreate.html#env_DB_CXX_NO_EXCEPTIONS" class="olink">DB_CXX_NO_EXCEPTIONS</a> flag with the <a href="../api_reference/CXX/env.html" class="olink">DbEnv</a> or <a href="../api_reference/CXX/db.html" class="olink">Db</a> constructors. When this flag is on, all C++ methods fail by returning an error code rather than throwing an exception.

4.  **I get unexpected results and database corruption when running threaded programs.**

    **I get error messages that mutex (for example, pthread_mutex_XXX or mutex_XXX) functions are undefined when linking applications with Berkeley DB.**

    On some architectures, the Berkeley DB library uses the ISO POSIX standard pthreads and UNIX International (UI) threads interfaces for underlying mutex support; Solaris is an example. You can specify compilers or compiler flags, or link with the appropriate thread library when loading your application to resolve the undefined references:

    ``` c
    cc ... -lpthread ...
    cc ... -lthread ...
    xlc_r ...
    cc ... -mt ...
    ```

    See the appropriate architecture-specific Reference Guide pages for more information.

    On systems where more than one type of mutex is available, it may be necessary for applications to use the same threads package from which Berkeley DB draws its mutexes. For example, if Berkeley DB was built to use the POSIX pthreads mutex calls for mutex support, the application may need to be written to use the POSIX pthreads interfaces for its threading model. This is only conjecture at this time, and although we know of no systems that actually have this requirement, it's not unlikely that some exist.

    In a few cases, Berkeley DB can be configured to use specific underlying mutex interfaces. You can use the <a href="build_unix_conf.md#build_unix_conf.--enable-posixmutexes" class="link">--enable-posixmutexes</a> and <a href="build_unix_conf.md#build_unix_conf.--enable-uimutexes" class="link">--enable-uimutexes</a> configuration options to specify the POSIX and Unix International (UI) threads packages. This should not, however, be necessary in most cases.

    In some cases, it is vitally important to make sure that you load the correct library. For example, on Solaris systems, there are POSIX pthread interfaces in the C library, so applications can link Berkeley DB using only C library and not see any undefined symbols. However, the C library POSIX pthread mutex support is insufficient for Berkeley DB, and Berkeley DB cannot detect that fact. Similar errors can arise when applications (for example, tclsh) use dlopen to dynamically load Berkeley DB as a library.

    If you are seeing problems in this area after you confirm that you're linking with the correct libraries, there are two other things you can try. First, if your platform supports interlibrary dependencies, we recommend that you change the Berkeley DB Makefile to specify the appropriate threads library when creating the Berkeley DB shared library, as an interlibrary dependency. Second, if your application is using dlopen to dynamically load Berkeley DB, specify the appropriate thread library on the link line when you load the application itself.

5.  **I get core dumps when running programs that fork children.**

    Berkeley DB handles should not be shared across process forks, each forked child should acquire its own Berkeley DB handles.

6.  **I get reports of uninitialized memory reads and writes when running software analysis tools (for example, Rational Software Corp.'s Purify tool).**

    For performance reasons, Berkeley DB does not write the unused portions of database pages or fill in unused structure fields. To turn off these errors when running software analysis tools, build with the <a href="build_unix_conf.md#build_unix_conf.--enable-umrw" class="link">--enable-umrw</a> configuration option.

7.  **Berkeley DB programs or the test suite fail unexpectedly.**

    The Berkeley DB architecture does not support placing the shared memory regions on remote filesystems -- for example, the Network File System (NFS) or the Andrew File System (AFS). For this reason, the shared memory regions (normally located in the database home directory) must reside on a local filesystem. See <a href="../../guides/programmer_reference/env_region.md" class="olink">Shared memory region</a> for more information.

    With respect to running the test suite, always check to make sure that TESTDIR is not on a remote mounted filesystem.

8.  **The <a href="../../api/c/db_dump.md" class="olink">db_dump</a> utility fails to build.**

    The <a href="../../api/c/db_dump.md" class="olink">db_dump185</a> utility is the utility that supports the conversion of Berkeley DB 1.85 and earlier databases to current database formats. If the build errors look something like the following, it means the db.h include file being loaded is not a Berkeley DB 1.85 version include file:

    ``` c
    db_dump185.c: In function `main':
    db_dump185.c:210: warning: assignment makes pointer from integer 
    without a cast
    db_dump185.c:212: warning: assignment makes pointer from integer 
    without a cast
    db_dump185.c:227: structure has no member named `seq'
    db_dump185.c:227: `R_NEXT' undeclared (first use in this function)
    ```

    If the build errors look something like the following, it means that the Berkeley DB 1.85 code was not found in the standard libraries:

    ``` c
    cc -o db_dump185 db_dump185.o
    ld:
    Unresolved:
    dbopen
    ```

    To build the <a href="../../api/c/db_dump.md" class="olink">db_dump185</a> utility, the Berkeley DB version 1.85 code must already been built and available on the system. If the Berkeley DB 1.85 header file is not found in a standard place, or if the library is not part of the standard libraries used for loading, you will need to edit your Makefile, and change the following lines:

    ``` c
    DB185INC=
    DB185LIB=
    ```

    So that the system Berkeley DB 1.85 header file and library are found; for example:

    ``` c
    DB185INC=/usr/local/include
    DB185LIB=-ldb185
    ```
