---
title: "Mac OS X"
api-name: "Mac OS X"
source: docs/installation/build_unix_macosx.html
---
## Mac OS X

1.  **When trying to link multiple Berkeley DB language interfaces (for example, Tcl, C++, Java, Python) into a single process, I get "multiple definitions" errors from dyld.**

    To fix this problem, set the environment variable MACOSX_DEPLOYMENT_TARGET to 10.3 (or your current version of OS X), and reconfigure and rebuild Berkeley DB from scratch. See the OS X ld(1) and dyld(1) man pages for information about how OS X handles symbol namespaces, as well as undefined and multiply-defined symbols.

2.  **When trying to use system-backed shared memory on OS X I see failures about "too many open files".**

    The default number of shared memory segments on OS X is too low. To fix this problem, edit the file /etc/rc, changing the kern.sysv.shmmax and kern.sysv.shmseg values as follows:

    ``` c
    *** /etc/rc.orig        Fri Dec 19 09:34:09 2003
    --- /etc/rc     Fri Dec 19 09:33:53 2003
    ***************
    *** 84,93 ****
       # System tuning
       sysctl -w kern.maxvnodes=$(echo $(sysctl -n hw.physmem) '33554432 /
    512 * 1024 +p'|dc)
    ! sysctl -w kern.sysv.shmmax=4194304
       sysctl -w kern.sysv.shmmin=1
       sysctl -w kern.sysv.shmmni=32
    ! sysctl -w kern.sysv.shmseg=8
       sysctl -w kern.sysv.shmall=1024
       if [ -f /etc/sysctl-macosxserver.conf ]; then
             awk '{ if (!-1 && -1) print $1 }' < 
    /etc/sysctl-macosxserver.conf | while read
    --- 84,93 ----
       # System tuning
       sysctl -w kern.maxvnodes=$(echo $(sysctl -n hw.physmem) '33554432 /
    512 * 1024 +p'|dc)
    ! sysctl -w kern.sysv.shmmax=134217728
       sysctl -w kern.sysv.shmmin=1
       sysctl -w kern.sysv.shmmni=32
    ! sysctl -w kern.sysv.shmseg=32
       sysctl -w kern.sysv.shmall=1024
       if [ -f /etc/sysctl-macosxserver.conf ]; then
             awk '{ if (!-1 && -1) print $1 }' <
     /etc/sysctl-macosxserver.conf | while read
    ```

    and then reboot the system.
