---
title: "DB_MPOOLFILE->sync()"
api-name: "DB_MPOOLFILE->sync()"
source: docs/api_reference/C/mempfsync.html
---
## DB_MPOOLFILE-\>sync()

``` c
#include <db.h>

int
DB_MPOOLFILE->sync(DB_MPOOLFILE *mpf);  
```

The `DB_MPOOLFILE->sync()` method writes all modified pages associated with the <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a> back to the source file. If any of the modified pages are <span class="emphasis">*pinned*</span> (that is, currently in use), `DB_MPOOLFILE->sync()` will ignore them.

The `DB_MPOOLFILE->sync()` method returns a non-zero error value on failure and 0 on success.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>
