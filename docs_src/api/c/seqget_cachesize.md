---
title: "DB_SEQUENCE->get_cachesize()"
api-name: "DB_SEQUENCE->get_cachesize()"
source: docs/api_reference/C/seqget_cachesize.html
---
## DB_SEQUENCE-\>get_cachesize()

``` c
#include <db.h>

int
DB_SEQUENCE->get_cachesize(DB_SEQUENCE *seq, u_int32_t *sizep);  
```

The `DB_SEQUENCE->get_cachesize()` method returns the current cache size.

The `DB_SEQUENCE->get_cachesize()` method may be called at any time during the life of the application.

The `DB_SEQUENCE->get_cachesize()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### sizep

The `DB_SEQUENCE->get_cachesize()` method returns the current cache size in **sizep**.

### Class

<a href="seq.md" class="link" title="Chapter 11.  The DB_SEQUENCE Handle">DB_SEQUENCE</a>

### See Also

<a href="seq.md#seqlist" class="xref" title="Sequences and Related Methods">Sequences and Related Methods</a>
