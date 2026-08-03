---
title: "DB_MPOOLFILE->set_priority()"
api-name: "DB_MPOOLFILE->set_priority()"
source: docs/api_reference/C/mempset_priority.html
---
## DB_MPOOLFILE-\>set_priority()

``` c
#include <db.h>

int
DB_MPOOLFILE->set_priority(DB_MPOOLFILE *mpf, DB_CACHE_PRIORITY priority); 
```

Set the cache priority for pages referenced by the <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a> handle.

The priority of a page biases the replacement algorithm to be more or less likely to discard a page when space is needed in the cache. The bias is temporary, and pages will eventually be discarded if they are not referenced again. The `DB_MPOOLFILE->set_priority()` method is only advisory, and does not guarantee pages will be treated in a specific way.

To set the priority for the pages belonging to a particular database, call the `DB_MPOOLFILE->set_priority()` method using the <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a> handle returned by the <a href="dbget_mpf.md" class="xref" title="DB-&gt;get_mpf()">DB-&gt;get_mpf()</a> method.

The `DB_MPOOLFILE->set_priority()` method configures a file in the cache, not only operations performed using the specified <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a> handle.

The `DB_MPOOLFILE->set_priority()` method may be called at any time during the life of the application.

The `DB_MPOOLFILE->set_priority()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### priority

The **priority** parameter must be set to one of the following values:

- `DB_PRIORITY_VERY_LOW`

  The lowest priority: pages are the most likely to be discarded.

- `DB_PRIORITY_LOW`

  The next lowest priority.

- `DB_PRIORITY_DEFAULT`

  The default priority.

- `DB_PRIORITY_HIGH`

  The next highest priority.

- `DB_PRIORITY_VERY_HIGH`

  The highest priority: pages are the least likely to be discarded.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>
