---
title: "DB->get_cachesize()"
api-name: "DB->get_cachesize()"
source: docs/api_reference/C/dbget_cachesize.html
---
## DB-\>get_cachesize()

``` c
#include <db.h>

int
DB->get_cachesize(DB *db,
    u_int32_t *gbytesp, u_int32_t *bytesp, int *ncachep);  
```

The `DB->get_cachesize()` method returns the current size and composition of the cache. These values may be set using the <a href="dbset_cachesize.md" class="xref" title="DB-&gt;set_cachesize()">DB-&gt;set_cachesize()</a> method.

The `DB->get_cachesize()` method may be called at any time during the life of the application.

The `DB->get_cachesize()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### gbytesp

The **gbytesp** parameter references memory into which the gigabytes of memory in the cache is copied.

#### bytesp

The **bytesp** parameter references memory into which the additional bytes of memory in the cache is copied.

#### ncachep

The **ncachep** parameter references memory into which the number of caches is copied.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>, <a href="dbset_cachesize.md" class="xref" title="DB-&gt;set_cachesize()">DB-&gt;set_cachesize()</a>
