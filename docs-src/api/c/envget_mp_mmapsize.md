---
title: "DB_ENV->get_mp_mmapsize()"
api-name: "DB_ENV->get_mp_mmapsize()"
source: docs/api_reference/C/envget_mp_mmapsize.html
---
## DB_ENV-\>get_mp_mmapsize()

``` c
#include <db.h>

int
DB_ENV->get_mp_mmapsize(DB_ENV *dbenv, size_t *mp_mmapsizep);  
```

The `DB_ENV->get_mp_mmapsize()` method returns the the maximum file size, in bytes, for a file to be mapped into the process address space. This value can be managed using the <a href="envset_mp_mmapsize.md" class="xref" title="DB_ENV-&gt;set_mp_mmapsize()">DB_ENV-&gt;set_mp_mmapsize()</a> method.

The `DB_ENV->get_mp_mmapsize()` method may be called at any time during the life of the application.

The `DB_ENV->get_mp_mmapsize()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### mp_mmapsizep

The `DB_ENV->get_mp_mmapsize()` method returns the maximum file map size in **mp_mmapsizep**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>, <a href="memp.md" class="link" title="Chapter 8.  The DB_MPOOLFILE Handle">DB_MPOOLFILE</a>

### See Also

<a href="memp.md#memplist" class="xref" title="Memory Pools and Related Methods">Memory Pools and Related Methods</a>, <a href="envset_mp_mmapsize.md" class="xref" title="DB_ENV-&gt;set_mp_mmapsize()">DB_ENV-&gt;set_mp_mmapsize()</a>
