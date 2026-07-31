---
title: "DB_MPOOLFILE->get_fileid()"
api-name: "DB_MPOOLFILE->get_fileid()"
source: docs/api_reference/C/mempget_fileid.html
---
## DB_MPOOLFILE-\>get_fileid()

``` c
#include <db.h>

int
DB_MPOOLFILE->get_fileid(DB_MPOOLFILE *mpf, u_int8_t *fileid);  
```

The `DB_MPOOLFILE->get_fileid()` method copies the file's identifier into the memory location referenced by **fileid**. The fileid specifies a unique identifier for the file, which is used so that the cache functions (that is, the shared memory buffer pool functions) are able to uniquely identify files. This is necessary for multiple processes wanting to share a file to correctly identify the file in the cache.

The `DB_MPOOLFILE->get_fileid()` method returns a non-zero error value on failure and 0 on success.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>, <a href="mempset_fileid.md" class="xref" title="DB_MPOOLFILE-&gt;set_fileid()">DB_MPOOLFILE-&gt;set_fileid()</a>
