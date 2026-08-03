---
title: "Hot Backup Changes"
api-name: "Hot Backup Changes"
source: docs/installation/upgrade_11gr2_52_hot_backup.html
---
## Hot Backup Changes

Because non-UNIX systems do not support atomic file system reads, the <a href="../../api/c/db_hotbackup.md" class="olink">db_hotbackup</a> utility has been modified to read data through the environment. If your application is running on a UNIX based system such as Solaris, HPUX, BSD or Mac OS, you can specify the **-F** flag to read directly from the filesystem. Please refer to <a href="../../guides/programmer_reference/transapp_recovery.md" class="olink">Recovery procedures</a> in the *Berkeley DB Programmer's Reference Guide* for more information on safely backing up your databases.
