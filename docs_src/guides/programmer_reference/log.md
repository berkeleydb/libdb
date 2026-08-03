---
title: "Chapter 17.  The Logging Subsystem"
api-name: "Chapter 17.  The Logging Subsystem"
source: docs/programmer_reference/log.html
---
## Chapter 17.  The Logging Subsystem

**Table of Contents**

<span class="sect1"> [Introduction to the logging subsystem](log.md#log_intro) </span>

<span class="sect1"> [Configuring logging](log_config.md) </span>

<span class="sect1"> [Log file limits](log_limits.md) </span>

## Introduction to the logging subsystem

The Logging subsystem is the logging facility used by Berkeley DB. It is largely Berkeley DB-specific, although it is potentially useful outside of the Berkeley DB package for applications wanting write-ahead logging support. Applications wanting to use the log for purposes other than logging file modifications based on a set of open file descriptors will almost certainly need to make source code modifications to the Berkeley DB code base.

A log can be shared by any number of threads of control. The <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> method is used to open a log. When the log is no longer in use, it should be closed using the <a href="../../api/c/envclose.md" class="olink">DB_ENV-&gt;close()</a> method.

Individual log entries are identified by log sequence numbers. Log sequence numbers are stored in an opaque object, an <a href="../../api/c/lsn.md" class="olink">DB_LSN</a>.

The <a href="../../api/c/logcursor.md" class="olink">DB_ENV-&gt;log_cursor()</a> method is used to allocate a log cursor. Log cursors have two methods: <a href="../../api/c/logcget.md" class="olink">DB_LOGC-&gt;get()</a> method to retrieve log records from the log, and <a href="../../api/c/logcclose.md" class="olink">DB_LOGC-&gt;close()</a> method to destroy the cursor.

There are additional methods for integrating the log subsystem with a transaction processing system:

<span class="term"> <a href="../../api/c/logflush.md" class="olink">DB_ENV-&gt;log_flush()</a> </span>  
Flushes the log up to a particular log sequence number.

<span class="term"> <a href="../../api/c/logcompare.md" class="olink">DB_ENV-&gt;log_compare()</a> </span>  
Allows applications to compare any two log sequence numbers.

<span class="term"> <a href="../../api/c/logfile.md" class="olink">DB_ENV-&gt;log_file()</a> </span>  
Maps a log sequence number to the specific log file that contains it.

<span class="term"> <a href="../../api/c/logarchive.md" class="olink">DB_ENV-&gt;log_archive()</a> </span>  
Returns various sets of log filenames. These methods are used for database administration; for example, to determine if log files may safely be removed from the system.

<span class="term"> <a href="../../api/c/logstat.md" class="olink">DB_ENV-&gt;log_stat()</a> </span>  
The display <a href="../../api/c/db_stat.md" class="olink">db_stat</a> utility used the <a href="../../api/c/logstat.md" class="olink">DB_ENV-&gt;log_stat()</a> method to display statistics about the log.

<span class="term"> <a href="../../api/c/envremove.md" class="olink">DB_ENV-&gt;remove()</a> </span>  
The log meta-information (but not the log files themselves) may be removed using the <a href="../../api/c/envremove.md" class="olink">DB_ENV-&gt;remove()</a> method.

For more information on the logging subsystem methods, see the <a href="../../api/c/lsn.md#loglist" class="olink">Logging Subsystem and Related Methods</a> section in the *Berkeley DB C API Reference Guide.*
