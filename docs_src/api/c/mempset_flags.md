---
title: "DB_MPOOLFILE->set_flags()"
api-name: "DB_MPOOLFILE->set_flags()"
source: docs/api_reference/C/mempset_flags.html
---
## DB_MPOOLFILE-\>set_flags()

``` c
#include <db.h>

int
DB_MPOOLFILE->set_flags(DB_MPOOLFILE *mpf, u_int32_t flags, int onoff)  
```

Configure a file in the cache.

To set the flags for a particular database, call the `DB_MPOOLFILE->set_flags()` method using the <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a> handle stored in the **mpf** field of the <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle.

The `DB_MPOOLFILE->set_flags()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flags

The **flags** parameter must be set by bitwise inclusively **OR**'ing together one or more of the following values:

- `DB_MPOOL_NOFILE`

  If set, no backing temporary file will be opened for the specified in-memory database, even if it expands to fill the entire cache. Attempts to create new database pages after the cache has been filled will fail.

  The `DB_MPOOL_NOFILE` flag configures a file in the cache, not only operations performed using the specified <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a> handle.

  The `DB_MPOOL_NOFILE` flag may be used to configure Berkeley DB at any time during the life of the application.

- `DB_MPOOL_UNLINK`

  If set, remove the file when the last reference to it is closed.

  The `DB_MPOOL_ULINK` flag configures a file in the cache, not only operations performed using the specified <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a> handle.

  The `DB_MPOOL_ULINK` flag may be used to configure Berkeley DB at any time during the life of the application.

#### onoff

If **onoff** is zero, the specified flags are cleared; otherwise they are set.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>
