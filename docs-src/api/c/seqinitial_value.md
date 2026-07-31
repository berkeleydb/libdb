---
title: "DB_SEQUENCE->initial_value()"
api-name: "DB_SEQUENCE->initial_value()"
source: docs/api_reference/C/seqinitial_value.html
---
## DB_SEQUENCE-\>initial_value()

``` c
#include <db.h>

int
DB_SEQUENCE->initial_value(DB_SEQUENCE *seq, db_seq_t value);  
```

Set the initial value for a sequence. This call is only effective when the sequence is being created.

The `DB_SEQUENCE->initial_value()` method may not be called after the <a href="seqopen.md" class="xref" title="DB_SEQUENCE-&gt;open()">DB_SEQUENCE-&gt;open()</a> method is called.

The `DB_SEQUENCE->initial_value()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### value

The initial value to set.

### Errors

The `DB_SEQUENCE->initial_value()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="seq.md" class="link" title="Chapter 11.  The DB_SEQUENCE Handle">DB_SEQUENCE</a>

### See Also

<a href="seq.md#seqlist" class="xref" title="Sequences and Related Methods">Sequences and Related Methods</a>
