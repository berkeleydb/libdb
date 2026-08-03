---
title: "DB_ENV->log_cursor()"
api-name: "DB_ENV->log_cursor()"
source: docs/api_reference/C/logcursor.html
---
## DB_ENV-\>log_cursor()

``` c
#include <db.h>

int
DB_ENV->log_cursor(DB_ENV *dbenv, DB_LOGC **cursorp, u_int32_t flags);  
```

The `DB_ENV->log_cursor()` method returns a created log cursor.

The `DB_ENV->log_cursor()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### cursorp

The **cursorp** parameter references memory into which a pointer to the created log cursor is copied.

#### flags

The **flags** parameter is currently unused, and must be set to 0.

### Errors

The `DB_ENV->log_cursor()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="logc.md" class="link" title="The DB_LOGC Handle">DB_LOGC</a>, <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a>

### See Also

<a href="lsn.md#loglist" class="xref" title="Logging Subsystem and Related Methods">Logging Subsystem and Related Methods</a>
