---
title: "DB_SEQUENCE->set_range()"
api-name: "DB_SEQUENCE->set_range()"
source: docs/api_reference/C/seqset_range.html
---
## DB_SEQUENCE-\>set_range()

``` c
#include <db.h>

int
DB_SEQUENCE->set_range(DB_SEQUENCE *seq, db_seq_t min, db_seq_t max);  
```

Configure a sequence range. This call is only effective when the sequence is being created. The range is limited to a signed 64 bit integer.

The `DB_SEQUENCE->set_range()` method may not be called after the <a href="seqopen.md" class="xref" title="DB_SEQUENCE-&gt;open()">DB_SEQUENCE-&gt;open()</a> method is called.

The `DB_SEQUENCE->set_range()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### min

Specifies the minimum value for the sequence.

#### max

Specifies the maximum value for the sequence.

### Errors

The `DB_SEQUENCE->set_range()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="seq.md" class="link" title="Chapter 11.  The DB_SEQUENCE Handle">DB_SEQUENCE</a>

### See Also

<a href="seq.md#seqlist" class="xref" title="Sequences and Related Methods">Sequences and Related Methods</a>
