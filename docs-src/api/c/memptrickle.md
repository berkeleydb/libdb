---
title: "DB_ENV->memp_trickle()"
api-name: "DB_ENV->memp_trickle()"
source: docs/api_reference/C/memptrickle.html
---
## DB_ENV-\>memp_trickle()

``` c
#include <db.h>

int
DB_ENV->memp_trickle(DB_ENV *env, int percent, int *nwrotep);  
```

The `DB_ENV->memp_trickle()` method ensures that a specified percent of the pages in the cache are clean, by writing dirty pages to their backing files.

The purpose of the `DB_ENV->memp_trickle()` function is to enable a memory pool manager to ensure that a page is always available for reading in new information without having to wait for a write.

The `DB_ENV->memp_trickle()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### percent

The **percent** parameter is the percent of the pages in the cache that should be clean.

#### nwrotep

The **nwrotep** parameter references memory into which the number of pages written to reach the specified percentage is copied.

### Errors

The `DB_ENV->memp_trickle()` method may fail and return one of the following non-zero errors: following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>
