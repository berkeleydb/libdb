---
title: "DB_MPOOLFILE->set_maxsize()"
api-name: "DB_MPOOLFILE->set_maxsize()"
source: docs/api_reference/C/mempset_maxsize.html
---
## DB_MPOOLFILE-\>set_maxsize()

``` c
#include <db.h>

int
DB_MPOOLFILE->set_maxsize(DB_MPOOLFILE *mpf,
    u_int32_t gbytes, u_int32_t bytes);  
```

Set the maximum size for the file to be **gbytes** gigabytes plus **bytes**. Attempts to allocate new pages in the file after the limit has been reached will fail.

To set the maximum file size for a particular database, call the `DB_MPOOLFILE->set_maxsize()` method using the <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a> handle stored in the **mpf** field of the <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle. Attempts to insert new items into the database after the limit has been reached may fail.

The `DB_MPOOLFILE->set_maxsize()` method configures a file in the cache, not only operations performed using the specified <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a> handle.

The `DB_MPOOLFILE->set_maxsize()` method may be called at any time during the life of the application.

The `DB_MPOOLFILE->set_maxsize()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### bytes

The maximum size of the file is set to **gbytes** gigabytes plus **bytes**.

#### gbytes

The maximum size of the file is set to **gbytes** gigabytes plus **bytes**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>
