---
title: "DB_MPOOLFILE->get_flags()"
api-name: "DB_MPOOLFILE->get_flags()"
source: docs/api_reference/C/mempget_flags.html
---
## DB_MPOOLFILE-\>get_flags()

``` c
#include <db.h>

int
DB_MPOOLFILE->get_flags(DB_MPOOLFILE *mpf, u_int32_t *flagsp);  
```

The `DB_MPOOLFILE->get_flags()` method returns the flags used to configure a file in the cache.

The `DB_MPOOLFILE->get_flags()` method may be called at any time during the life of the application.

The `DB_MPOOLFILE->get_flags()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flagsp

The `DB_MPOOLFILE->get_flags()` method returns the flags in **flagsp**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>, <a href="mempset_flags.md" class="xref" title="DB_MPOOLFILE-&gt;set_flags()">DB_MPOOLFILE-&gt;set_flags()</a>
