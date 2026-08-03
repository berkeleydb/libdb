---
title: "DB_SEQUENCE->get_range()"
api-name: "DB_SEQUENCE->get_range()"
source: docs/api_reference/C/seqget_range.html
---
## DB_SEQUENCE-\>get_range()

``` c
#include <db.h>

int
DB_SEQUENCE->get_range(DB_SEQUENCE *seq, db_seq_t *minp, db_seq_t *maxp);  
```

The `DB_SEQUENCE->get_range()` method returns the range of values in the sequence.

The `DB_SEQUENCE->get_range()` method may be called at any time during the life of the application.

The `DB_SEQUENCE->get_range()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### minp

The `DB_SEQUENCE->get_range()` method returns the minimum value in **minp**.

#### maxp

The `DB_SEQUENCE->get_range()` method returns the maximum value in **maxp**.

### Class

<a href="seq.md" class="link" title="Chapter 11.  The DB_SEQUENCE Handle">DB_SEQUENCE</a>

### See Also

<a href="seq.md#seqlist" class="xref" title="Sequences and Related Methods">Sequences and Related Methods</a>
