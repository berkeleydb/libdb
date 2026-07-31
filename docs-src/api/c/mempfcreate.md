---
title: "DB_ENV->memp_fcreate()"
api-name: "DB_ENV->memp_fcreate()"
source: docs/api_reference/C/mempfcreate.html
---
## DB_ENV-\>memp_fcreate()

``` c
#include <db.h>

int
DB_ENV->memp_fcreate(DB_ENV *dbenvp, DB_MPOOLFILE **dbmfp, 
                     u_int32_t flags);  
```

The `DB_ENV->memp_fcreate()` method creates a <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a> structure that is the handle for a Berkeley DB cache (that is, a shared memory buffer pool file). A pointer to this structure is returned in the memory to which **dbmfp** refers. Calling the <a href="mempfclose.md" class="xref" title="DB_MPOOLFILE-&gt;close()">DB_MPOOLFILE-&gt;close()</a> method will discard the returned handle.

The `DB_ENV->memp_fcreate()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### dbmfp

The `DB_ENV->memp_fcreate()` method returns a pointer to a mpool structure in **dbmfp**.

#### flags

The **flags** parameter is currently unused, and must be set to 0.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>
