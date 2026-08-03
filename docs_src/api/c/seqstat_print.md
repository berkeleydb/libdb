---
title: "DB_SEQUENCE->stat_print()"
api-name: "DB_SEQUENCE->stat_print()"
source: docs/api_reference/C/seqstat_print.html
---
## DB_SEQUENCE-\>stat_print()

``` c
#include <db.h>

int
DB_SEQUENCE->stat_print(DB_SEQUENCE *db, u_int32_t flags);  
```

The `DB_SEQUENCE->stat_print()` method prints diagnostic information to the output channel described by the <a href="envset_msgfile.md" class="xref" title="DB_ENV-&gt;set_msgfile()">DB_ENV-&gt;set_msgfile()</a> method.

The `DB_SEQUENCE->stat_print()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flags

The **flags** parameter must be set by bitwise inclusively **OR**'ing together one or more of the following values:

- `DB_STAT_CLEAR`

  Reset statistics after printing their values.

### Class

<a href="seq.md" class="link" title="Chapter 11.  The DB_SEQUENCE Handle">DB_SEQUENCE</a>

### See Also

<a href="seq.md#seqlist" class="xref" title="Sequences and Related Methods">Sequences and Related Methods</a>
