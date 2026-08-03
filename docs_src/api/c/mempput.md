---
title: "DB_MPOOLFILE->put()"
api-name: "DB_MPOOLFILE->put()"
source: docs/api_reference/C/mempput.html
---
## DB_MPOOLFILE-\>put()

``` c
#include <db.h>

int
DB_MPOOLFILE->put(DB_MPOOLFILE *mpf,
    void *pgaddr, DB_CACHE_PRIORITY priority, u_int32_t flags);  
```

The `DB_MPOOLFILE->put()` method returns a reference to a page in the cache, setting the priority of the page as specified by the **priority** parameter.

The `DB_MPOOLFILE->put()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### pgaddr

The **pgaddr** parameter is the address of the page to be returned to the cache. The **pgaddr** parameter must be a value previously returned by the <a href="mempfget.md" class="xref" title="DB_MPOOLFILE-&gt;get()">DB_MPOOLFILE-&gt;get()</a> method.

#### priority

Set the page's **priority** as follows:

- `DB_PRIORITY_UNCHANGED`

  The priority is unchanged.

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

#### flags

The **flags** parameter is currently unused, and must be set to 0.

### Errors

The `DB_MPOOLFILE->put()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>
