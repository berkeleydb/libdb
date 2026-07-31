---
title: "DB_ENV->log_stat()"
api-name: "DB_ENV->log_stat()"
source: docs/api_reference/C/logstat.html
---
## DB_ENV-\>log_stat()

``` c
#include <db.h>

int
DB_ENV->log_stat(DB_ENV *env, DB_LOG_STAT **statp, u_int32_t flags);  
```

The `DB_ENV->log_stat()` method returns the logging subsystem statistics.

The `DB_ENV->log_stat()` method creates a statistical structure of type `DB_LOG_STAT` and copies a pointer to it into a user-specified memory location.

Statistical structures are stored in allocated memory. If application-specific allocation routines have been declared (see <a href="envset_alloc.md" class="xref" title="DB_ENV-&gt;set_alloc()">DB_ENV-&gt;set_alloc()</a> for more information), they are used to allocate the memory; otherwise, the standard C library **malloc**(3) is used. The caller is responsible for deallocating the memory. To deallocate the memory, free the memory reference; references inside the returned memory need not be individually freed.

The following `DB_LOG_STAT` fields will be filled in:

- **u_int32_t st_cur_file;**

  The current log file number.

- **u_int32_t st_cur_offset;**

  The byte offset in the current log file.

- **u_int32_t st_disk_file;**

  The log file number of the last record known to be on disk.

- **u_int32_t st_disk_offset;**

  The byte offset of the last record known to be on disk.

- **u_int32_t st_fileid_init;**

  The initial allocated file logging identifiers.

- **u_int32_t st_lg_bsize;**

  The in-memory log record cache size.

- **u_int32_t st_lg_size;**

  The log file size.

- **u_int32_t st_magic;**

  The magic number that identifies a file as a log file.

- **u_int32_t st_maxcommitperflush;**

  The maximum number of commits contained in a single log flush.

- **u_int32_t st_maxnfileid;**

  The maximum number of file logging identifiers used.

- **u_int32_t st_mincommitperflush;**

  The minimum number of commits contained in a single log flush that contained a commit.

- **int st_mode;**

  The mode of any created log files.

- **u_int32_t st_nfileid;**

  The current number of file logging identifiers.

- **uintmax_t st_rcount;**

  The number of times the log has been read from disk.

- **uintmax_t st_record;**

  The number of records written to this log.

- **roff_t st_regsize;**

  The region size, in bytes.

- **uintmax_t st_region_wait;**

  The number of times that a thread of control was forced to wait before obtaining the log region mutex.

- **uintmax_t st_region_nowait;**

  The number of times that a thread of control was able to obtain the log region mutex without waiting.

- **uintmax_t st_scount;**

  The number of times the log has been flushed to disk.

- **u_int32_t st_version;**

  The version of the log file type.

- **u_int32_t st_w_bytes;**

  The number of bytes over and above **st_w_mbytes** written to this log.

- **u_int32_t st_w_mbytes;**

  The number of megabytes written to this log.

- **u_int32_t st_wc_bytes;**

  The number of bytes over and above **st_wc_mbytes** written to this log since the last checkpoint.

- **u_int32_t st_wc_mbytes;**

  The number of megabytes written to this log since the last checkpoint.

- **uintmax_t st_wcount_fill;**

  The number of times the log has been written to disk because the in-memory log record cache filled up.

- **uintmax_t st_wcount;**

  The number of times the log has been written to disk.

The `DB_ENV->log_stat()` method may not be called before the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called.

The `DB_ENV->log_stat()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### statp

The **statp** parameter references memory into which a pointer to the allocated statistics structure is copied.

#### flags

The **flags** parameter must be set to 0 or the following value:

- `DB_STAT_CLEAR`

  Reset statistics after returning their values.

### Errors

The `DB_ENV->log_stat()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="logc.md" class="link" title="The DB_LOGC Handle">DB_LOGC</a>, <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a>

### See Also

<a href="lsn.md#loglist" class="xref" title="Logging Subsystem and Related Methods">Logging Subsystem and Related Methods</a>
