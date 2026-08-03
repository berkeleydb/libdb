---
title: "DB_MPOOLFILE->set_ftype()"
api-name: "DB_MPOOLFILE->set_ftype()"
source: docs/api_reference/C/mempset_ftype.html
---
## DB_MPOOLFILE-\>set_ftype()

``` c
#include <db.h>

int
DB_MPOOLFILE->set_ftype(DB_MPOOLFILE *mpf, int ftype);  
```

The `DB_MPOOLFILE->set_ftype()` method specifies a file type for the purposes of input or output processing of the file's pages as they are read from or written to, the backing filesystem store.

The `DB_MPOOLFILE->set_ftype()` method configures a file in the cache, not only operations performed using the specified <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a> handle.

The `DB_MPOOLFILE->set_ftype()` method may not be called after the <a href="mempfopen.md" class="xref" title="DB_MPOOLFILE-&gt;open()">DB_MPOOLFILE-&gt;open()</a> method is called. If the file is already open in the cache when <a href="mempfopen.md" class="xref" title="DB_MPOOLFILE-&gt;open()">DB_MPOOLFILE-&gt;open()</a> is called, the information specified to `DB_MPOOLFILE->set_ftype()` will replace the existing information.

The `DB_MPOOLFILE->set_ftype()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### ftype

The **ftype** parameter sets the file's type for the purposes of input and output processing. The **ftype** must be the same as a **ftype** parameter previously specified to the <a href="mempregister.md" class="xref" title="DB_ENV-&gt;memp_register()">DB_ENV-&gt;memp_register()</a> method.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>
