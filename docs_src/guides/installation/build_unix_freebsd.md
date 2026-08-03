---
title: "FreeBSD"
api-name: "FreeBSD"
source: docs/installation/build_unix_freebsd.html
---
## FreeBSD

1.  **I can't compile and run multithreaded applications.**

    Special compile-time flags are required when compiling threaded applications on FreeBSD. If you are compiling a threaded application, you must compile with the \_THREAD_SAFE and -pthread flags:

    ``` c
    cc -D_THREAD_SAFE -pthread ...
    ```

    The Berkeley DB library will automatically build with the correct options.

2.  **I see fsync and close system call failures when accessing databases or log files on NFS-mounted filesystems.**

    Some FreeBSD releases are known to return ENOLCK from fsync and close calls on NFS-mounted filesystems, even though the call has succeeded. The Berkeley DB code should be modified to ignore ENOLCK errors, or no Berkeley DB files should be placed on NFS-mounted filesystems on these systems.
