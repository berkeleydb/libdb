---
title: "DB_MPOOLFILE->get_maxsize()"
api-name: "DB_MPOOLFILE->get_maxsize()"
source: docs/api_reference/C/mempget_maxsize.html
---
## DB_MPOOLFILE-\>get_maxsize()

``` c
#include <db.h>

int
DB_MPOOLFILE->get_maxsize(DB_MPOOLFILE *mpf,
    u_int32_t *gbytesp, u_int32_t *bytesp);  
```

Returns the maximum size configured for the file, as configured using the <a href="mempset_maxsize.md" class="xref" title="DB_MPOOLFILE-&gt;set_maxsize()">DB_MPOOLFILE-&gt;set_maxsize()</a> method.

The `DB_MPOOLFILE->get_maxsize()` method returns a non-zero error value on failure and 0 on success.

The `DB_MPOOLFILE->get_maxsize()` method may be called at any time during the life of the application.

### Parameters

#### gbytesp

The **gbytesp** parameter references memory into which the gigabytes of memory in the maximum file size is copied.

#### bytesp

The **bytesp** parameter references memory into which the additional bytes of memory in the maximum file size is copied.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>, <a href="mempset_maxsize.md" class="xref" title="DB_MPOOLFILE-&gt;set_maxsize()">DB_MPOOLFILE-&gt;set_maxsize()</a>
