---
title: "Configuring logging"
api-name: "Configuring logging"
source: docs/programmer_reference/log_config.html
---
## Configuring logging

The aspects of logging that may be configured are the size of the logging subsystem's region, the size of the log files on disk and the size of the log buffer in memory. The <a href="../../api/c/envset_lg_regionmax.md" class="olink">DB_ENV-&gt;set_lg_regionmax()</a> method specifies the size of the logging subsystem's region, in bytes. The logging subsystem's default size is approximately 60KB. This value may need to be increased if a large number of files are registered with the Berkeley DB log manager, for example, by opening a large number of Berkeley DB database files in a transactional application.

The <a href="../../api/c/envset_lg_max.md" class="olink">DB_ENV-&gt;set_lg_max()</a> method specifies the individual log file size for all the applications sharing the Berkeley DB environment. Setting the log file size is largely a matter of convenience and a reflection of the application's preferences in backup media and frequency. However, setting the log file size too low can potentially cause problems because it would be possible to run out of log sequence numbers, which requires a full archival and application restart to reset. See <a href="log_limits.md" class="xref" title="Log file limits">Log file limits</a> for more information.

The <a href="../../api/c/envset_lg_bsize.md" class="olink">DB_ENV-&gt;set_lg_bsize()</a> method specifies the size of the in-memory log buffer, in bytes. Log information is stored in memory until the buffer fills up or transaction commit forces the buffer to be written to disk. Larger buffer sizes can significantly increase throughput in the presence of long-running transactions, highly concurrent applications, or transactions producing large amounts of data. By default, the buffer is approximately 32KB.

The <a href="../../api/c/envset_lg_dir.md" class="olink">DB_ENV-&gt;set_lg_dir()</a> method specifies the directory in which log files will be placed. By default, log files are placed in the environment home directory.

The <a href="../../api/c/envset_lg_filemode.md" class="olink">DB_ENV-&gt;set_lg_filemode()</a> method specifies the absolute file mode for created log files. This method is only useful for the rare Berkeley DB application that does not control its umask value.

The <a href="../../api/c/envlog_set_config.md" class="olink">DB_ENV-&gt;log_set_config()</a> method configures several boolean parameters that control the use of file system controls such as O_DIRECT and O_DSYNC, automatic removal of log files, in-memory logging, and pre-zeroing of logfiles.
