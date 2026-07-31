---
title: "QNX"
api-name: "QNX"
source: docs/installation/build_unix_qnx.html
---
## QNX

1.  **To what versions of QNX has DB been ported?**

    Berkeley DB has been ported to the QNX Neutrino technology which is commonly referred to as QNX RTP (Real-Time Platform). Berkeley DB has not been ported to earlier versions of QNX, such as QNX 4.25.

2.  **Building Berkeley DB shared libraries fails.**

    The `/bin/sh` utility distributed with some QNX releases drops core when running the GNU libtool script (which is used to build Berkeley DB shared libraries). There are two workarounds for this problem: First, only build static libraries. You can disable building shared libraries by specifying the configuration flag when configuring Berkeley DB.

    Second, build Berkeley DB using an alternate shell. QNX distributions include an accessories disk with additional tools. One of the included tools is the GNU bash shell, which is able to run the libtool script. To build Berkeley DB using an alternate shell, move `/bin/sh` aside, link or copy the alternate shell into that location, configure, build and install Berkeley DB, and then replace the original shell utility.

3.  **Are there any QNX filesystem issues?**

    Berkeley DB generates temporary files for use in transactionally protected file system operations. Due to the filename length limit of 48 characters in the QNX filesystem, applications that are using transactions should specify a database name that is at most 43 characters.

4.  **What are the implications of QNX's requirement to use `shm_open`(2) in order to use `mmap`(2)?**

    QNX requires that files mapped with `mmap`(2) be opened using `shm_open`(2). There are other places in addition to the environment shared memory regions, where Berkeley DB tries to memory map files if it can.

    The memory pool subsystem normally attempts to use `mmap`(2) even when using private memory, as indicated by the <a href="../../api/c/envopen.md#envopen_DB_PRIVATE" class="olink">DB_PRIVATE</a> flag to <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a>. In the case of QNX, if an application is using private memory, Berkeley DB will not attempt to map the memory and will instead use the local cache.

5.  **What are the implications of QNX's mutex implementation using microkernel resources?**

    On QNX, the primitives implementing mutexes consume system resources. Therefore, if an application unexpectedly fails, those resources could leak. Berkeley DB solves this problem by always allocating mutexes in the persistent shared memory regions. Then, if an application fails, running recovery or explicitly removing the database environment by calling the <a href="../../api/c/envremove.md" class="olink">DB_ENV-&gt;remove()</a> method will allow Berkeley DB to release those previously held mutex resources. If an application specifies the <a href="../../api/c/envopen.md#envopen_DB_PRIVATE" class="olink">DB_PRIVATE</a> flag (choosing not to use persistent shared memory), and then fails, mutexes allocated in that private memory may leak their underlying system resources. Therefore, the <a href="../../api/c/envopen.md#envopen_DB_PRIVATE" class="olink">DB_PRIVATE</a> flag should be used with caution on QNX.

6.  **The make clean command fails to execute when building the Berkeley DB SQL interface.**

    Remove the build directory manually to clean up and proceed.
