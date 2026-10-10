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

3.  **A second process gets `Invalid argument` and the environment panics (`BDB0061 PANIC: Invalid argument`).**

    Typical messages are `pthread lock failed: Invalid argument` or `pthread suspend failed: Invalid argument`, followed by `DB_RUNRECOVERY`. It happens when a process opens an environment **after the process that created it has exited**. A common case is a loader that populates the environment and exits, followed by a separate server that attaches.

    The cause is a documented property of FreeBSD's threads library, not an environment defect. `libthr(3)` says that *"process-shared objects require initialization in each process that use them"*. libthr keeps each process-shared mutex as a kernel object attached to the backing file. With the default `kern.ipc.umtx_vnode_persistent=0`, that object is destroyed when the last process unmaps the file. libdb's environment regions are files (`__db.001`, …), and its mutexes are process-shared pthread objects. So once the creator exits and nothing else has the region mapped, the next process finds mutexes it cannot lock. Linux is not affected.

    Either of these avoids it:

    - Keep the kernel objects alive after last close:

      ``` sh
      sysctl kern.ipc.umtx_vnode_persistent=1
      ```

      To make this permanent, add `kern.ipc.umtx_vnode_persistent=1` to `/etc/sysctl.conf`.

    - Build libdb with test-and-set mutexes, which do not use pthread objects at all:

      ``` sh
      ../dist/configure --with-mutex=x86_64/gcc-assembly
      ```

    Both were measured on FreeBSD 14.5: an environment created by an exited process, then driven by 1 to 8 threads, ran with no panics either way. A probe that reproduces the underlying behaviour without libdb is in `test/bench/pshared_attach_probe.c`.

4.  **Throughput collapsed above one thread in releases before 2026.10.4.**

    Earlier releases destroyed and re-created a process-shared mutex on every transaction and on every contended lock release. On FreeBSD each such destroy walks every live process-shared object in the process (`pshared_gc` in libthr), and libdb keeps one per cached page, so each destroy was proportional to the cache size. v2026.10.4 removes both call sites, and a 32-thread workload runs about 15 times faster. Upgrade rather than tune around it.
