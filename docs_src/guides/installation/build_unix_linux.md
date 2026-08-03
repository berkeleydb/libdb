---
title: "Linux"
api-name: "Linux"
source: docs/installation/build_unix_linux.html
---
## Linux

1.  **I can't compile and run multithreaded applications.**

    Special compile-time flags are required when compiling threaded applications on Linux. If you are compiling a threaded application, you must compile with the \_REENTRANT flag:

    ``` c
    cc -D_REENTRANT ...
    ```

    The Berkeley DB library will automatically build with the correct options.

2.  **I see database corruption when accessing databases.**

    Some Linux filesystems do not support POSIX filesystem semantics. Specifically, ext2 and early releases of ReiserFS, and ext3 in some configurations, do not support "ordered data mode" and may insert random data into database or log files when systems crash. Berkeley DB files should not be placed on a filesystem that does not support, or is not configured to support, POSIX semantics.

3.  **What scheduler should I use?**

    In some Linux kernels you can select schedulers, and the default is the "anticipatory" scheduler. We recommend not using the "anticipatory" scheduler for transaction processing workloads.
