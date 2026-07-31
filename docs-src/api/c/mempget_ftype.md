---
title: "DB_MPOOLFILE->get_ftype()"
api-name: "DB_MPOOLFILE->get_ftype()"
source: docs/api_reference/C/mempget_ftype.html
---
## DB_MPOOLFILE-\>get_ftype()

``` c
#include <db.h>

int
DB_MPOOLFILE->get_ftype(DB_MPOOLFILE *mpf, int *ftypep);  
```

The `DB_MPOOLFILE->get_ftype()` method returns the file type. The file type is used for the purposes of file processing, and will be the same as is set using the <a href="mempregister.md" class="xref" title="DB_ENV-&gt;memp_register()">DB_ENV-&gt;memp_register()</a> method.

The `DB_MPOOLFILE->get_ftype()` method may be called at any time during the life of the application.

The `DB_MPOOLFILE->get_ftype()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### ftypep

The `DB_MPOOLFILE->get_ftype()` method returns the file type in **ftypep**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>, <a href="mempset_ftype.md" class="xref" title="DB_MPOOLFILE-&gt;set_ftype()">DB_MPOOLFILE-&gt;set_ftype()</a>
