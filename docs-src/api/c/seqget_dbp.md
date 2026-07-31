---
title: "DB_SEQUENCE->get_dbp()"
api-name: "DB_SEQUENCE->get_dbp()"
source: docs/api_reference/C/seqget_dbp.html
---
## DB_SEQUENCE-\>get_dbp()

``` c
#include <db.h>

int
DB_SEQUENCE->get_dbp(DB_SEQUENCE *seq, DB **dbp);  
```

The `DB_SEQUENCE->get_dbp()` method returns the database handle used by the sequence.

The `DB_SEQUENCE->get_dbp()` method may be called at any time during the life of the application.

The `DB_SEQUENCE->get_dbp()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### dbp

The **dbp** parameter references memory into which a pointer to the database handle is copied.

### Class

<a href="seq.md" class="link" title="Chapter 11.  The DB_SEQUENCE Handle">DB_SEQUENCE</a>

### See Also

<a href="seq.md#seqlist" class="xref" title="Sequences and Related Methods">Sequences and Related Methods</a>
