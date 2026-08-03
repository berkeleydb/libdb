---
title: "Using Recovery on Journaling Filesystems"
api-name: "Using Recovery on Journaling Filesystems"
source: docs/programmer_reference/transapp_journal.html
---
## Using Recovery on Journaling Filesystems

In some cases, the use of meta-data only journaling file systems can lead to log file corruption. The window of vulnerability is quite small, but if the operating system experiences an unclean shutdown while Berkeley DB is creating a new log file, it is possible that upon file system recovery, the system will be in a state where the log file has been created, but its own meta-data has not.

When a log file is corrupted to this degree, normal recovery can fail and your application may be unable to open your environment. Instead, an error something like this is issued when you attempt to run normal recovery on environment open:

``` c
    Ignoring log file: /var/dblog/log.0000000074: magic number 
    6c73732f, not 40988
    Invalid log file: log.0000000074: Invalid argument
    PANIC: Invalid argument
    process-private: unable to find environment
    txn_checkpoint interface requires an environment configured for 
    the transaction subsystem  
```

In this case, it may be possible to successfully recover the environment by ignoring the log file that was being created — to do this, rename the log file with the highest number to a temporary name:

``` c
 mv DBHOME/log.000000XXX my-temporary-log-file  
```

and try running normal environment recovery again. If recovery is successful, and your application is able to open the environment, then you can delete the log file that you renamed.

If recovery is not successful, then you must perform a catastrophic recovery from a previous backup.

This situation has been shown to occur when using ext3 in writeback mode, but other journaling filesystems could exhibit similar behavior.

To be absolutely certain of your application's ability to recover your environment in the event of a system crash, either use non-journaling filesystems, or use a journaling filesystem in a safe (albeit slower) configuration, such as ext3 in ordered mode.
