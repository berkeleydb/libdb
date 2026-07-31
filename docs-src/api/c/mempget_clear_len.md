---
title: "DB_MPOOLFILE->get_clear_len()"
api-name: "DB_MPOOLFILE->get_clear_len()"
source: docs/api_reference/C/mempget_clear_len.html
---
## DB_MPOOLFILE-\>get_clear_len()

``` c
#include <db.h>

int
DB_MPOOLFILE->get_clear_len(DB_MPOOLFILE *mpf, u_int32_t *lenp);  
```

The `DB_MPOOLFILE->get_clear_len()` method returns the bytes to be cleared.

The `DB_MPOOLFILE->get_clear_len()` method may be called at any time during the life of the application.

The `DB_MPOOLFILE->get_clear_len()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### lenp

The `DB_MPOOLFILE->get_clear_len()` method returns the bytes to be cleared in **lenp**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>
