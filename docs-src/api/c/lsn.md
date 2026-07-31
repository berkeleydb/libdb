---
title: "Chapter 7.  The DB_LSN Handle"
api-name: "Chapter 7.  The DB_LSN Handle"
source: docs/api_reference/C/lsn.html
---
## Chapter 7.  The DB_LSN Handle

``` c
#include <db.h>

typedef struct __typedef struct __db_lsn DB_LSN; ;  
```

The `DB_LSN` object is a <span class="emphasis">*log sequence number*</span> which specifies a unique location in a log file. A `DB_LSN` consists of two unsigned 32-bit integers -- one specifies the log file number, and the other specifies an offset in the log file.

## Logging Subsystem and Related Methods

<table data-border="1" width="80%">
<thead>
<tr>
<th>Logging Subsystem and Related Methods</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr>
<td><a href="logarchive.md" class="xref" title="DB_ENV-&gt;log_archive()">DB_ENV-&gt;log_archive()</a></td>
<td>List log and database files</td>
</tr>
<tr>
<td><a href="logfile.md" class="xref" title="DB_ENV-&gt;log_file()">DB_ENV-&gt;log_file()</a></td>
<td>Map Log Sequence Numbers to log files</td>
</tr>
<tr>
<td><a href="logflush.md" class="xref" title="DB_ENV-&gt;log_flush()">DB_ENV-&gt;log_flush()</a></td>
<td>Flush log records</td>
</tr>
<tr>
<td><a href="logprintf.md" class="xref" title="DB_ENV-&gt;log_printf()">DB_ENV-&gt;log_printf()</a></td>
<td>Append informational message to the log</td>
</tr>
<tr>
<td><a href="logput.md" class="xref" title="DB_ENV-&gt;log_put()">DB_ENV-&gt;log_put()</a></td>
<td>Write a log record</td>
</tr>
<tr>
<td><a href="logstat.md" class="xref" title="DB_ENV-&gt;log_stat()">DB_ENV-&gt;log_stat()</a></td>
<td>Return log subsystem statistics</td>
</tr>
<tr>
<td><a href="logstat_print.md" class="xref" title="DB_ENV-&gt;log_stat_print()">DB_ENV-&gt;log_stat_print()</a></td>
<td>Print log subsystem statistics</td>
</tr>
<tr>
<td><a href="logcompare.md" class="xref" title="log_compare">log_compare</a></td>
<td>Compare two Log Sequence Numbers</td>
</tr>
<tr>
<td colspan="2"><strong>Logging Subsystem Cursors</strong></td>
</tr>
<tr>
<td><a href="logcursor.md" class="xref" title="DB_ENV-&gt;log_cursor()">DB_ENV-&gt;log_cursor()</a></td>
<td>Create a log cursor handle</td>
</tr>
<tr>
<td><a href="logc.md" class="xref" title="The DB_LOGC Handle">The DB_LOGC Handle</a></td>
<td>A log cursor handle</td>
</tr>
<tr>
<td><a href="logcclose.md" class="xref" title="DB_LOGC-&gt;close()">DB_LOGC-&gt;close()</a></td>
<td>Close a log cursor</td>
</tr>
<tr>
<td><a href="logcget.md" class="xref" title="DB_LOGC-&gt;get()">DB_LOGC-&gt;get()</a></td>
<td>Retrieve a log record</td>
</tr>
<tr>
<td colspan="2"><strong>Logging Subsystem Configuration</strong></td>
</tr>
<tr>
<td><a href="envlog_set_config.md" class="xref" title="DB_ENV-&gt;log_set_config()">DB_ENV-&gt;log_set_config()</a>, <a href="envlog_get_config.md" class="xref" title="DB_ENV-&gt;log_get_config()">DB_ENV-&gt;log_get_config()</a></td>
<td>Configure the logging subsystem</td>
</tr>
<tr>
<td><a href="envset_lg_bsize.md" class="xref" title="DB_ENV-&gt;set_lg_bsize()">DB_ENV-&gt;set_lg_bsize()</a>, <a href="envget_lg_bsize.md" class="xref" title="DB_ENV-&gt;get_lg_bsize()">DB_ENV-&gt;get_lg_bsize()</a></td>
<td>Set/get log buffer size</td>
</tr>
<tr>
<td><a href="envset_lg_dir.md" class="xref" title="DB_ENV-&gt;set_lg_dir()">DB_ENV-&gt;set_lg_dir()</a>, <a href="envget_lg_dir.md" class="xref" title="DB_ENV-&gt;get_lg_dir()">DB_ENV-&gt;get_lg_dir()</a></td>
<td>Set/get the environment logging directory</td>
</tr>
<tr>
<td><a href="envset_lg_filemode.md" class="xref" title="DB_ENV-&gt;set_lg_filemode()">DB_ENV-&gt;set_lg_filemode()</a>, <a href="envget_lg_filemode.md" class="xref" title="DB_ENV-&gt;get_lg_filemode()">DB_ENV-&gt;get_lg_filemode()</a></td>
<td>Set/get log file mode</td>
</tr>
<tr>
<td><a href="envset_lg_max.md" class="xref" title="DB_ENV-&gt;set_lg_max()">DB_ENV-&gt;set_lg_max()</a>, <a href="envget_lg_max.md" class="xref" title="DB_ENV-&gt;get_lg_max()">DB_ENV-&gt;get_lg_max()</a></td>
<td>Set/get log file size</td>
</tr>
<tr>
<td><a href="envset_lg_regionmax.md" class="xref" title="DB_ENV-&gt;set_lg_regionmax()">DB_ENV-&gt;set_lg_regionmax()</a>, <a href="envget_lg_regionmax.md" class="xref" title="DB_ENV-&gt;get_lg_regionmax()">DB_ENV-&gt;get_lg_regionmax()</a></td>
<td>Set/get logging region size</td>
</tr>
</tbody>
</table>
