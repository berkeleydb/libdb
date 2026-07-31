---
title: "DB_SEQUENCE->close()"
api-name: "DB_SEQUENCE->close()"
source: docs/api_reference/C/seqclose.html
---
## DB_SEQUENCE-\>close()

``` c
#include <db.h>

int
DB_SEQUENCE->close(DB_SEQUENCE *seq, u_int32_t flags);  
```

The `DB_SEQUENCE->close()` method closes the sequence handle. Any unused cached values are lost.

The <a href="seq.md" class="link" title="Chapter 11.  The DB_SEQUENCE Handle">DB_SEQUENCE</a> handle may not be accessed again after `DB_SEQUENCE->close()` is called, regardless of its return.

The `DB_SEQUENCE->close()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flags

The **flags** parameter is currently unused, and must be set to 0.

### Errors

The `DB_SEQUENCE->close()` method method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="seq.md" class="link" title="Chapter 11.  The DB_SEQUENCE Handle">DB_SEQUENCE</a>

### See Also

<a href="seq.md#seqlist" class="xref" title="Sequences and Related Methods">Sequences and Related Methods</a>
