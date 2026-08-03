---
title: "DB_MPOOLFILE->get_priority()"
api-name: "DB_MPOOLFILE->get_priority()"
source: docs/api_reference/C/mempget_priority.html
---
## DB_MPOOLFILE-\>get_priority()

``` c
#include <db.h>

int
DB_MPOOLFILE->get_priority(DB_MPOOLFILE *mpf, 
                           DB_CACHE_PRIORITY *priorityp); 
```

The `DB_MPOOLFILE->get_priority()` method returns the cache priority for the file referenced by the <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a> handle. The priority of a page biases the replacement algorithm to be more or less likely to discard a page when space is needed in the cache. This value is set using the <a href="mempset_priority.md" class="xref" title="DB_MPOOLFILE-&gt;set_priority()">DB_MPOOLFILE-&gt;set_priority()</a> method.

The `DB_MPOOLFILE->get_priority()` method may be called at any time during the life of the application.

The `DB_MPOOLFILE->get_priority()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### priorityp

The `DB_MPOOLFILE->get_priority()` method returns a reference to the cache priority for the file referenced by the <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a> handle in **priorityp**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>, <a href="mempset_priority.md" class="xref" title="DB_MPOOLFILE-&gt;set_priority()">DB_MPOOLFILE-&gt;set_priority()</a>
