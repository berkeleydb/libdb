---
title: "Log file removal"
api-name: "Log file removal"
source: docs/programmer_reference/transapp_logfile.html
---
## Log file removal

The fourth component of the infrastructure, log file removal, concerns the ongoing disk consumption of the database log files. Depending on the rate at which the application writes to the databases and the available disk space, the number of log files may increase quickly enough so that disk space will be a resource problem. For this reason, you will periodically want to remove log files in order to conserve disk space. This procedure is distinct from database and log file archival for catastrophic recovery, and you cannot remove the current log files simply because you have created a database snapshot or copied log files to archival media.

Log files may be removed at any time, as long as:

- the log file is not involved in an active transaction.
- a checkpoint has been written subsequent to the log file's creation.
- the log file is not the only log file in the environment.

Additionally, when Replication Manager is running the log file is older than the most out of date active site in the replication group.

If you are preparing for catastrophic failure, you will want to copy the log files to archival media before you remove them as described in <a href="transapp_archival.md" class="xref" title="Database and log file archival">Database and log file archival</a>.

If you are not preparing for catastrophic failure, any one of the following methods can be used to remove log files:

1.  Run the standalone <a href="../../api/c/db_archive.md" class="olink">db_archive</a> utility with the **-d** option, to remove any log files that are no longer needed at the time the command is executed.
2.  Call the <a href="../../api/c/logarchive.md" class="olink">DB_ENV-&gt;log_archive()</a> method from the application, with the <a href="../../api/c/logarchive.md#archive_DB_ARCH_REMOVE" class="olink">DB_ARCH_REMOVE</a> flag, to remove any log files that are no longer needed at the time the call is made.
3.  Call the <a href="../../api/c/envlog_set_config.md" class="olink">DB_ENV-&gt;log_set_config()</a> method from the application, with the <a href="../../api/c/envlog_set_config.md#log_set_config_DB_LOG_AUTO_REMOVE" class="olink">DB_LOG_AUTO_REMOVE</a> flag, to remove any log files that are no longer needed on an ongoing basis. With this configuration, Berkeley DB will automatically remove log files, and the application will not have an opportunity to copy the log files to backup media.
