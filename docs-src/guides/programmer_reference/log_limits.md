---
title: "Log file limits"
api-name: "Log file limits"
source: docs/programmer_reference/log_limits.html
---
## Log file limits

Log filenames and sizes impose a limit on how long databases may be used in a Berkeley DB database environment. It is quite unlikely that an application will reach this limit; however, if the limit is reached, the Berkeley DB environment's databases must be dumped and reloaded.

The log filename consists of **log.** followed by 10 digits, with a maximum of 2,000,000,000 log files. Consider an application performing 6000 transactions per second for 24 hours a day, logged into 10MB log files, in which each transaction is logging approximately 500 bytes of data. The following calculation:

``` c
(10 * 2^20 * 2000000000) / (6000 * 500 * 365 * 60 * 60 * 24) = ~221
```

indicates that the system will run out of log filenames in roughly 221 years.

There is no way to reset the log filename space in Berkeley DB. If your application is reaching the end of its log filename space, you must do the following:

1.  Archive your databases as if to prepare for catastrophic failure (see <a href="transapp_archival.md" class="xref" title="Database and log file archival">Database and log file archival</a> for more information).
2.  Reset the database's log sequence numbers (see the **-r** option to the <a href="../../api/c/db_load.md" class="olink">db_load</a> utility for more information).
3.  Remove all of the log files from the database environment. (This is the only situation in which all the log files are removed from an environment; in all other cases, at least a single log file is retained.)
4.  Restart your application.
