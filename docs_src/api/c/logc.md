---
title: "The DB_LOGC Handle"
api-name: "The DB_LOGC Handle"
source: docs/api_reference/C/logc.html
---
## The DB_LOGC Handle

``` c
#include <db.h>

typedef struct __typedef struct __db_log_cursor DB_LOGC;  
```

The `DB_LOGC` object is the handle for a cursor into the log files, supporting sequential access to the records stored in log files. The handle is not free-threaded. Once the <a href="logcclose.md" class="xref" title="DB_LOGC-&gt;close()">DB_LOGC-&gt;close()</a> method is called, the handle may not be accessed again, regardless of that method's return.

For more information, see the <a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a> handle.
