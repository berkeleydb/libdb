---
title: "Windows notes"
api-name: "Windows notes"
source: docs/installation/build_win_notes.html
---
## Windows notes

If a system memory environment is closed by all processes, subsequent attempts to open it will return an error. To successfully open a transactional environment in this state, recovery must be run by the next process to open the environment. For non-transactional environments, applications should remove the existing environment and then create a new database environment.

1.  Berkeley DB does not support the Windows/95, Windows/98 or Windows/ME platforms.

2.  On Windows, system paging file memory is freed on last close. For this reason, multiple processes sharing a database environment created using the <a href="../../api/c/envopen.md#envopen_DB_SYSTEM_MEM" class="olink">DB_SYSTEM_MEM</a> flag must arrange for at least one process to always have the environment open, or alternatively that any process joining the environment be prepared to re-create it.

3.  When using the <a href="../../api/c/envopen.md#envopen_DB_SYSTEM_MEM" class="olink">DB_SYSTEM_MEM</a> flag, Berkeley DB shared regions are created without ACLs, which means that the regions are only accessible to a single user. If wider sharing is appropriate (for example, both user applications and Windows/NT service applications need to access the Berkeley DB regions), the Berkeley DB code will need to be modified to create the shared regions with the correct ACLs. Alternatively, by not specifying the <a href="../../api/c/envopen.md#envopen_DB_SYSTEM_MEM" class="olink">DB_SYSTEM_MEM</a> flag, filesystem-backed regions will be created instead, and the permissions on those files may be directly specified through the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> method.

4.  Applications that operate on wide character strings can use the Windows function WideCharToMultiByte with the code page CP_UTF8 to convert paths to the form expected by Berkeley DB. Internally, Berkeley DB calls MultiByteToWideChar on paths before calling Windows functions.

5.  Various Berkeley DB methods take a **mode** argument, which is intended to specify the underlying file permissions for created files. Berkeley DB currently ignores this argument on Windows systems.

    It would be possible to construct a set of security attributes to pass to **CreateFile** that accurately represents the mode. In the worst case, this would involve looking up user and all group names, and creating an entry for each. Alternatively, we could call the **\_chmod** (partial emulation) function after file creation, although this leaves us with an obvious race.

    Practically speaking, however, these efforts would be largely meaningless on a FAT file system, which only has a "readable" and "writable" flag, applying to all users.
