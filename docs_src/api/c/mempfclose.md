---
title: "DB_MPOOLFILE->close()"
api-name: "DB_MPOOLFILE->close()"
source: docs/api_reference/C/mempfclose.html
---
## DB_MPOOLFILE-\>close()

``` c
#include <db.h>

int
DB_MPOOLFILE->close(DB_MPOOLFILE *mpf, u_int32_t flags);  
```

The `DB_MPOOLFILE->close()` method closes the source file indicated by the <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a> structure. Calling `DB_MPOOLFILE->close()` does not imply a call to <a href="mempfsync.md" class="xref" title="DB_MPOOLFILE-&gt;sync()">DB_MPOOLFILE-&gt;sync()</a>; that is, no pages are written to the source file as as a result of calling `DB_MPOOLFILE->close.()`.

If the <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a> was temporary, any underlying files created for this <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a> will be removed.

After `DB_MPOOLFILE->close()` has been called, regardless of its return, the <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a> handle may not be accessed again.

The `DB_MPOOLFILE->close()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flags

The **flags** parameter is currently unused, and must be set to 0.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>
