---
title: "DB_MPOOLFILE->set_lsn_offset()"
api-name: "DB_MPOOLFILE->set_lsn_offset()"
source: docs/api_reference/C/mempset_lsn_offset.html
---
## DB_MPOOLFILE-\>set_lsn_offset()

``` c
#include <db.h>

int
DB_MPOOLFILE->set_lsn_offset(DB_MPOOLFILE *mpf, int32_t lsn_offset);  
```

The `DB_MPOOLFILE->set_lsn_offset()` method specifies the zero-based byte offset of a log sequence number (<a href="lsn.md" class="link" title="Chapter 7.  The DB_LSN Handle">DB_LSN</a>) on the file's pages, for the purposes of page-flushing as part of transaction checkpoint. (See the <a href="mempsync.md" class="xref" title="DB_ENV-&gt;memp_sync()">DB_ENV-&gt;memp_sync()</a> documentation for more information.)

The `DB_MPOOLFILE->set_lsn_offset()` method configures a file in the cache, not only operations performed using the specified <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a> handle.

The `DB_MPOOLFILE->set_lsn_offset()` method may not be called after the <a href="mempfopen.md" class="xref" title="DB_MPOOLFILE-&gt;open()">DB_MPOOLFILE-&gt;open()</a> method is called. If the file is already open in the cache when <a href="mempfopen.md" class="xref" title="DB_MPOOLFILE-&gt;open()">DB_MPOOLFILE-&gt;open()</a> is called, the information specified to `DB_MPOOLFILE->set_lsn_offset()` must be consistent with the existing file or an error will be returned.

The `DB_MPOOLFILE->set_lsn_offset()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### lsn_offset

The **lsn_offset** parameter is the zero-based byte offset of the log sequence number on the file's pages.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>
