---
title: "DB_ENV->memp_sync()"
api-name: "DB_ENV->memp_sync()"
source: docs/api_reference/C/mempsync.html
---
## DB_ENV-\>memp_sync()

``` c
#include <db.h>

int
DB_ENV->memp_sync(DB_ENV *env, DB_LSN *lsn);  
```

The `DB_ENV->memp_sync()` method flushes modified pages in the cache to their backing files.

Pages in the cache that cannot be immediately written back to disk (for example, pages that are currently in use by another thread of control) are waited for and written to disk as soon as it is possible to do so.

The `DB_ENV->memp_sync()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### lsn

The purpose of the **lsn** parameter is to enable a transaction manager to ensure, as part of a checkpoint, that all pages modified by a certain time have been written to disk.

All modified pages with a a log sequence number (<a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a>) less than the **lsn** parameter are written to disk. If **lsn** is NULL, all modified pages in the cache are written to disk.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>
