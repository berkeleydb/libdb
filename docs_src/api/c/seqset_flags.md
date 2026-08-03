---
title: "DB_SEQUENCE->set_flags()"
api-name: "DB_SEQUENCE->set_flags()"
source: docs/api_reference/C/seqset_flags.html
---
## DB_SEQUENCE-\>set_flags()

``` c
#include <db.h>

int
DB_SEQUENCE->set_flags(DB_SEQUENCE *seq, u_int32_t flags);  
```

Configure a sequence. The flags are only effective when creating a sequence. Calling `DB_SEQUENCE->set_flags()` is additive; there is no way to clear flags.

The `DB_SEQUENCE->set_flags()` method may not be called after the <a href="seqopen.md" class="xref" title="DB_SEQUENCE-&gt;open()">DB_SEQUENCE-&gt;open()</a> method is called.

The `DB_SEQUENCE->set_flags()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flags

The **flags** parameter must be set to 0 or by bitwise inclusively **OR**'ing together one or more of the following values:

- `DB_SEQ_DEC`

  Specify that the sequence should be decremented.

- `DB_SEQ_INC`

  Specify that the sequence should be incremented. This is the default.

- `DB_SEQ_WRAP`

  Specify that the sequence should wrap around when it is incremented (decremented) past the specified maximum (minimum) value.

### Errors

The `DB_SEQUENCE->set_flags()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="seq.md" class="link" title="Chapter 11.  The DB_SEQUENCE Handle">DB_SEQUENCE</a>

### See Also

<a href="seq.md#seqlist" class="xref" title="Sequences and Related Methods">Sequences and Related Methods</a>
