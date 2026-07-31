---
title: "Compile-time configuration"
api-name: "Compile-time configuration"
source: docs/installation/debug_compile.html
---
## Compile-time configuration

There are three compile-time configuration options that assist in debugging Berkeley DB and Berkeley DB applications:

<span class="term"> <a href="build_unix_conf.md#build_unix_conf.--enable-debug" class="link">--enable-debug</a> </span>  
If you want to build Berkeley DB with **-g** as the C and C++ compiler flag, enter --enable-debug as an argument to configure. This will create Berkeley DB with debugging symbols, as well as load various Berkeley DB routines that can be called directly from a debugger to display database page content, cursor queues, and so forth. (Note that the **-O** optimization flag will still be specified. To compile with only the **-g**, explicitly set the `CFLAGS` environment variable before configuring.)

<span class="term"> <a href="build_unix_conf.md#build_unix_conf.--enable-diagnostic" class="link">--enable-diagnostic</a> </span>  
If you want to build Berkeley DB with debugging run-time sanity checks and with DIAGNOSTIC \#defined during compilation, enter --enable-diagnostic as an argument to configure. This will cause a number of special checks to be performed when Berkeley DB is running. This flag should not be defined when configuring to build production binaries because it degrades performance.

<span class="term"> <a href="build_unix_conf.md#build_unix_conf.--enable-umrw" class="link">--enable-umrw</a> </span>  
When compiling Berkeley DB for use in run-time memory consistency checkers (in particular, programs that look for reads and writes of uninitialized memory), use --enable-umrw as an argument to configure. This guarantees, among other things, that Berkeley DB will completely initialize allocated pages rather than initializing only the minimum necessary amount.
