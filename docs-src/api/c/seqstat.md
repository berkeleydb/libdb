---
title: "DB_SEQUENCE->stat()"
api-name: "DB_SEQUENCE->stat()"
source: docs/api_reference/C/seqstat.html
---
## DB_SEQUENCE-\>stat()

``` c
#include <db.h>

int
DB_SEQUENCE->stat(DB_SEQUENCE *db, DB_SEQUENCE_STAT **spp, 
                  u_int32_t flags); 
```

The `DB_SEQUENCE->stat()` method creates a statistical structure and copies a pointer to it into user-specified memory locations. Specifically, if **spp** is non-NULL, a pointer to the statistics for the database are copied into the memory location to which it refers.

Statistical structures are stored in allocated memory. If application-specific allocation routines have been declared (see <a href="envset_alloc.md" class="xref" title="DB_ENV-&gt;set_alloc()">DB_ENV-&gt;set_alloc()</a> for more information), they are used to allocate the memory; otherwise, the standard C library **malloc**(3) is used. The caller is responsible for deallocating the memory. To deallocate the memory, free the memory reference; references inside the returned memory need not be individually freed.

In the presence of multiple threads or processes accessing an active sequence, the information returned by `DB_SEQUENCE->stat()` may be out-of-date.

The `DB_SEQUENCE->stat()` method cannot be transaction-protected. For this reason, it should be called in a thread of control that has no open cursors or active transactions.

The `DB_SEQUENCE->stat()` method returns a non-zero error value on failure and 0 on success.

The statistics are stored in a structure of type `DB_SEQUENCE_STAT`. The following fields will be filled in:

- **int32_t st_cache_size;**

  The number of values that will be cached in this handle.

- **db_seq_t st_current;**

  The current value of the sequence in the database.

- **u_int32_t st_flags;**

  The flags value for the sequence.

- **db_seq_t st_last_value;**

  The last cached value of the sequence.

- **db_seq_t st_min;**

  The minimum permitted value of the sequence.

- **db_seq_t st_max;**

  The maximum permitted value of the sequence.

- **uintmax_t st_nowait;**

  The number of times that a thread of control was able to obtain handle mutex without waiting.

- **db_seq_t st_value;**

  The current cached value of the sequence.

- **uintmax_t st_wait;**

  The number of times a thread of control was forced to wait on the handle mutex.

### Parameters

#### flags

The **flags** parameter must be set by bitwise inclusively **OR**'ing together one or more of the following values:

- `DB_STAT_ALL`

  Display all available information.

- `DB_STAT_CLEAR`

  Reset statistics after printing their values.

### Class

<a href="seq.md" class="link" title="Chapter 11.  The DB_SEQUENCE Handle">DB_SEQUENCE</a>

### See Also

<a href="seq.md#seqlist" class="xref" title="Sequences and Related Methods">Sequences and Related Methods</a>
