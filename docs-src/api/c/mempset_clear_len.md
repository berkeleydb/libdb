---
title: "DB_MPOOLFILE->set_clear_len()"
api-name: "DB_MPOOLFILE->set_clear_len()"
source: docs/api_reference/C/mempset_clear_len.html
---
## DB_MPOOLFILE-\>set_clear_len()

``` c
#include <db.h>

int
DB_MPOOLFILE->set_clear_len(DB_MPOOLFILE *mpf, u_int32_t len);  
```

The `DB_MPOOLFILE->set_clear_len()` method sets the number of initial bytes in a page that should be set to nul when the page is created as a result of the <a href="mempfget.md#mpoolfget_DB_MPOOL_CREATE" class="link">DB_MPOOL_CREATE</a> or <a href="mempfget.md#mpoolfget_DB_MPOOL_NEW" class="link">DB_MPOOL_NEW</a> flags specified to <a href="mempfget.md" class="xref" title="DB_MPOOLFILE-&gt;get()">DB_MPOOLFILE-&gt;get()</a>. If no clear length is specified, the entire page is cleared when it is created.

The `DB_MPOOLFILE->set_clear_len()` method configures a file in the cache, not only operations performed using the specified <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a> handle.

The `DB_MPOOLFILE->set_clear_len()` method may not be called after the <a href="mempfopen.md" class="xref" title="DB_MPOOLFILE-&gt;open()">DB_MPOOLFILE-&gt;open()</a> method is called. If the file is already open in the cache when <a href="mempfopen.md" class="xref" title="DB_MPOOLFILE-&gt;open()">DB_MPOOLFILE-&gt;open()</a> is called, the information specified to `DB_MPOOLFILE->set_clear_len()` must be consistent with the existing file or an error will be returned.

The `DB_MPOOLFILE->set_clear_len()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### len

The **len** parameter is the number of initial bytes in a page that should be set to nul when the page is created. A value of 0 results in the entire page being set to nul bytes.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>
