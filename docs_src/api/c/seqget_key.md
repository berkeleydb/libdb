---
title: "DB_SEQUENCE->get_key()"
api-name: "DB_SEQUENCE->get_key()"
source: docs/api_reference/C/seqget_key.html
---
## DB_SEQUENCE-\>get_key()

``` c
#include <db.h>

int
DB_SEQUENCE->get_key(DB_SEQUENCE *seq, DBT *key);  
```

The `DB_SEQUENCE->get_key()` method returns the key for the sequence.

The `DB_SEQUENCE->get_key()` method may be called at any time during the life of the application.

The `DB_SEQUENCE->get_key()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### key

The **key** parameter references memory into which a pointer to the key data is copied.

### Class

<a href="seq.md" class="link" title="Chapter 11.  The DB_SEQUENCE Handle">DB_SEQUENCE</a>

### See Also

<a href="seq.md#seqlist" class="xref" title="Sequences and Related Methods">Sequences and Related Methods</a>
