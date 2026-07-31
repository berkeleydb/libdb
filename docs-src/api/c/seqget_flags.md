---
title: "DB_SEQUENCE->get_flags()"
api-name: "DB_SEQUENCE->get_flags()"
source: docs/api_reference/C/seqget_flags.html
---
## DB_SEQUENCE-\>get_flags()

``` c
#include <db.h>

int
DB_SEQUENCE->get_flags(DB_SEQUENCE *seq, u_int32_t *flagsp);  
```

The `DB_SEQUENCE->get_flags()` method returns the current flags.

The `DB_SEQUENCE->get_flags()` method may be called at any time during the life of the application.

The `DB_SEQUENCE->get_flags()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flagsp

The `DB_SEQUENCE->get_flags()` method returns the current flags in **flagsp**.

### Class

<a href="seq.md" class="link" title="Chapter 11.  The DB_SEQUENCE Handle">DB_SEQUENCE</a>

### See Also

<a href="seq.md#seqlist" class="xref" title="Sequences and Related Methods">Sequences and Related Methods</a>
