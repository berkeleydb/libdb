---
title: "DB_SEQUENCE->set_cachesize()"
api-name: "DB_SEQUENCE->set_cachesize()"
source: docs/api_reference/C/seqset_cachesize.html
---
## DB_SEQUENCE-\>set_cachesize()

``` c
#include <db.h>

int
DB_SEQUENCE->set_cachesize(DB_SEQUENCE *seq, int32_t size);  
```

Configure the number of elements cached by a sequence handle.

The `DB_SEQUENCE->set_cachesize()` method may not be called after the <a href="seqopen.md" class="xref" title="DB_SEQUENCE-&gt;open()">DB_SEQUENCE-&gt;open()</a> method is called.

The `DB_SEQUENCE->set_cachesize()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### size

The number of elements in the cache.

### Errors

The `DB_SEQUENCE->set_cachesize()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="seq.md" class="link" title="Chapter 11.  The DB_SEQUENCE Handle">DB_SEQUENCE</a>

### See Also

<a href="seq.md#seqlist" class="xref" title="Sequences and Related Methods">Sequences and Related Methods</a>
