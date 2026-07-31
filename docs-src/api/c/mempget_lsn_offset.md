---
title: "DB_MPOOLFILE->get_lsn_offset()"
api-name: "DB_MPOOLFILE->get_lsn_offset()"
source: docs/api_reference/C/mempget_lsn_offset.html
---
## DB_MPOOLFILE-\>get_lsn_offset()

``` c
#include <db.h>

int
DB_MPOOLFILE->get_lsn_offset(DB_MPOOLFILE *mpf, int32_t *lsn_offsetp);  
```

The `DB_MPOOLFILE->get_lsn_offset()` method returns the log sequence number byte offset configured for a file's pages using the <a href="mempset_lsn_offset.md" class="xref" title="DB_MPOOLFILE-&gt;set_lsn_offset()">DB_MPOOLFILE-&gt;set_lsn_offset()</a> method.

The `DB_MPOOLFILE->get_lsn_offset()` method may be called at any time during the life of the application.

The `DB_MPOOLFILE->get_lsn_offset()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### lsn_offsetp

The `DB_MPOOLFILE->get_lsn_offset()` method returns the log sequence number byte offset in **lsn_offsetp**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>, <a href="mempset_lsn_offset.md" class="xref" title="DB_MPOOLFILE-&gt;set_lsn_offset()">DB_MPOOLFILE-&gt;set_lsn_offset()</a>
