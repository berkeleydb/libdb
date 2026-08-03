---
title: "db_sequence_create"
api-name: "db_sequence_create"
source: docs/api_reference/C/seqcreate.html
---
## db_sequence_create

``` c
#include <db.h>

int db_sequence_create(DB_SEQUENCE **seq, DB *db, u_int32_t flags);  
```

Creates a sequence handle, which can then be opened with <a href="seqopen.md" class="xref" title="DB_SEQUENCE-&gt;open()">DB_SEQUENCE-&gt;open()</a>.

`DB_SEQUENCE` handles are free-threaded if the <a href="seqopen.md#seqopen_DB_THREAD" class="link">DB_THREAD</a> flag is specified to the <a href="seqopen.md" class="xref" title="DB_SEQUENCE-&gt;open()">DB_SEQUENCE-&gt;open()</a> method when the sequence is opened. Once the <a href="seqclose.md" class="xref" title="DB_SEQUENCE-&gt;close()">DB_SEQUENCE-&gt;close()</a> or <a href="seqremove.md" class="xref" title="DB_SEQUENCE-&gt;remove()">DB_SEQUENCE-&gt;remove()</a> methods are called, the handle can not be accessed again, regardless of the method's return.

Each handle opened on a sequence may maintain a separate cache of values which are returned to the application using the <a href="seqget.md" class="xref" title="DB_SEQUENCE-&gt;get()">DB_SEQUENCE-&gt;get()</a> method either singly or in groups depending on its **delta** parameter.

Calling the <a href="seqclose.md" class="xref" title="DB_SEQUENCE-&gt;close()">DB_SEQUENCE-&gt;close()</a> or <a href="seqremove.md" class="xref" title="DB_SEQUENCE-&gt;remove()">DB_SEQUENCE-&gt;remove()</a> methods discards this handle.

`db_sequence_create()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### seq

The **seq** parameter references the memory into which the returned structure pointer is stored.

#### db

The **db** parameter is an open database handle which holds the persistent data for the sequence. The database may be of any type, but must not have been configured to support duplicate data items.

#### flags

The **flags** parameter is currently unused, and must be set to 0.

### Errors

The `db_sequence_create` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="seq.md" class="link" title="Chapter 11.  The DB_SEQUENCE Handle">DB_SEQUENCE</a>

### See Also

<a href="seq.md#seqlist" class="xref" title="Sequences and Related Methods">Sequences and Related Methods</a>
