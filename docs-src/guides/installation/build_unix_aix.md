---
title: "AIX"
api-name: "AIX"
source: docs/installation/build_unix_aix.html
---
## AIX

1.  **I can't compile and run multithreaded applications.**

    Special compile-time flags are required when compiling threaded applications on AIX. If you are compiling a threaded application, you must compile with the \_THREAD_SAFE flag and load with specific libraries; for example, "-lc_r". Specifying the compiler name with a trailing "\_r" usually performs the right actions for the system.

    ``` c
    xlc_r ...
    cc -D_THREAD_SAFE -lc_r ...
    ```

    The Berkeley DB library will automatically build with the correct options.

2.  **I can't run using the <a href="../../api/c/envopen.md#envopen_DB_SYSTEM_MEM" class="olink">DB_SYSTEM_MEM</a> option to <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a>.**

    AIX 4.1 allows applications to map only 10 system shared memory segments. In AIX 4.3, this has been raised to 256K segments, but only if you set the environment variable "export EXTSHM=ON".

3.  **On AIX 4.3.2 (or before) I see duplicate symbol warnings when building the C++ shared library and when linking applications.**

    We are aware of some duplicate symbol warnings with this platform, but they do not appear to affect the correct operation of applications.

4.  **On AIX 4.3.3 I see undefined symbols for DbEnv::set_error_stream, Db::set_error_stream or DbEnv::verify when linking C++ applications. (These undefined symbols also appear when building the Berkeley DB C++ example applications).**

    By default, Berkeley DB is built with \_LARGE_FILES set to 1 to support the creation of "large" database files. However, this also affects how standard classes, like iostream, are named internally. When building your application, use a "-D_LARGE_FILES=1" compilation option, or insert "#define \_LARGE_FILES 1" before any \#include statements.

5.  **I can't create database files larger than 1GB on AIX.**

    If you're running on AIX 4.1 or earlier, try changing the source code for `os/os_open.c` to always specify the **O_LARGEFILE** flag to the `open`(2) system call, and recompile Berkeley DB from scratch.

    Also, the documentation for the IBM Visual Age compiler states that it does not not support the 64-bit filesystem APIs necessary for creating large files; the ibmcxx product must be used instead. We have not heard whether the GNU gcc compiler supports the 64-bit APIs or not.

    Finally, to create large files under AIX, the filesystem has to be configured to support large files and the system wide user hard-limit for file sizes has to be greater than 1GB.

6.  **I see errors about "open64" when building Berkeley DB applications.**

    System include files (most commonly fcntl.h) in some releases of AIX and Solaris redefine "open" when large-file support is enabled for applications. This causes problems when compiling applications because "open" is a method in the Berkeley DB APIs. To work around this problem:

    1.  Avoid including the problematical system include files in source code files which also include Berkeley DB include files and call into the Berkeley DB API.
    2.  Before building Berkeley DB, modify the generated include file db.h to itself include the problematical system include files.
    3.  Turn off Berkeley DB large-file support by specifying the <a href="build_unix_conf.md#build_unix_conf.--disable-largefile" class="link">--disable-largefile</a> configuration option and rebuilding.

7.  **I see the error "Redeclaration of lseek64" when building Berkeley DB with the --enable-sql and --enable-test options.**

    In some releases of AIX, the system include files (most commonly `unistd.h`) redefine `lseek` to `lseek64` when large-file support is enabled even though `lseek` may have already been defined when the `_LARGE_FILE_API` macro is on. To work around this problem, do either one of the following:

    1.  Disable large-file support in Berkeley DB by specifying the `--disable-largefile` configuration option and rebuilding.
    2.  Edit `db.h` manually after running the configure command, and remove the line that includes `unistd.h`.
