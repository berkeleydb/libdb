---
title: "DB_ENV->log_file()"
api-name: "DB_ENV->log_file()"
source: docs/api_reference/C/logfile.html
---
## DB_ENV-\>log_file()

``` c
#include <db.h>

int
DB_ENV->log_file(DB_ENV *env,
    const DB_LSN *lsn, char *namep, size_t len);  
```

The `DB_ENV->log_file()` method maps `DB_LSN` structures to filenames, returning the name of the file containing the record named by **lsn**.

This mapping of `DB_LSN` structures to files is needed for database administration. For example, a transaction manager typically records the earliest <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a> needed for restart, and the database administrator may want to archive log files to tape when they contain only <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a> entries before the earliest one needed for restart.

The `DB_ENV->log_file()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### lsn

The **lsn** parameter is the `DB_LSN` structure for which a filename is wanted.

#### namep

The **namep** parameter references memory into which the name of the file containing the record named by **lsn** is copied.

#### len

The **len** parameter is the length of the **namep** buffer in bytes. If **namep** is too short to hold the filename, `DB_ENV->log_file()` will fail. (Log filenames are always 14 characters long.)

### Errors

The `DB_ENV->log_file()` method may fail and return one of the following non-zero errors:

#### EINVAL

If supplied buffer was too small to hold the log filename; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="logc.md" class="link" title="The DB_LOGC Handle">DB_LOGC</a>, <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a>

### See Also

<a href="lsn.md#loglist" class="xref" title="Logging Subsystem and Related Methods">Logging Subsystem and Related Methods</a>
