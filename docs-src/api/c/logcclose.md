---
title: "DB_LOGC->close()"
api-name: "DB_LOGC->close()"
source: docs/api_reference/C/logcclose.html
---
## DB_LOGC-\>close()

``` c
#include <db.h>

int
DB_LOGC->close(DB_LOGC *cursor, u_int32_t flags);  
```

The `DB_LOGC->close()` method discards the log cursor. After `DB_LOGC->close()` has been called, regardless of its return, the cursor handle may not be used again.

The `DB_LOGC->close()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flags

The **flags** parameter is currently unused, and must be set to 0.

### Errors

The `DB_LOGC->close()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the cursor is already closed; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="logc.md" class="link" title="The DB_LOGC Handle">DB_LOGC</a>, <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a>

### See Also

<a href="lsn.md#loglist" class="xref" title="Logging Subsystem and Related Methods">Logging Subsystem and Related Methods</a>
