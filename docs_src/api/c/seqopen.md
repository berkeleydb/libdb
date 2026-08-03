---
title: "DB_SEQUENCE->open()"
api-name: "DB_SEQUENCE->open()"
source: docs/api_reference/C/seqopen.html
---
## DB_SEQUENCE-\>open()

``` c
#include <db.h>

int
DB_SEQUENCE->open(DB_SEQUENCE *seq, DB_TXN *txnid, DBT *key, 
                  u_int32_t flags); 
```

The `DB_SEQUENCE->open()` method opens the sequence represented by the **key**. The key must be compatible with the underlying database specified in the corresponding call to <a href="seqcreate.md" class="xref" title="db_sequence_create">db_sequence_create</a>.

The `DB_SEQUENCE->open()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### key

The **key** specifies which record in the database stores the persistent sequence data.

#### flags

The **flags** parameter must be set to 0 or by bitwise inclusively **OR**'ing together one or more of the following values:

- `DB_CREATE`

  Create the sequence. If the sequence does not already exist and the `DB_CREATE` flag is not specified, the `DB_SEQUENCE->open()` method will fail.

- `DB_EXCL`

  Return an error if the sequence already exists. This flag is only meaningful when specified with the `DB_CREATE` flag.

- `DB_THREAD`

  Cause the <a href="seq.md" class="link" title="Chapter 11.  The DB_SEQUENCE Handle">DB_SEQUENCE</a> handle returned by `DB_SEQUENCE->open()` to be <span class="emphasis">*free-threaded*</span>; that is, usable by multiple threads within a single address space. Note that if multiple threads create multiple sequences using the same database handle that handle must have been opened specifying this flag.

#### txnid

If the operation is part of an application-specified transaction, the **txnid** parameter is a transaction handle returned from <a href="txnbegin.md" class="xref" title="DB_ENV-&gt;txn_begin()">DB_ENV-&gt;txn_begin()</a>; if the operation is part of a Berkeley DB Concurrent Data Store group, the **txnid** parameter is a handle returned from <a href="envcdsgroup_begin.md" class="xref" title="DB_ENV-&gt;cdsgroup_begin()">DB_ENV-&gt;cdsgroup_begin()</a>; otherwise NULL. If no transaction handle is specified, but the operation occurs in a transactional database, the operation will be implicitly transaction protected. Transactionally protected operations on a <a href="seq.md" class="link" title="Chapter 11.  The DB_SEQUENCE Handle">DB_SEQUENCE</a> handle require the <a href="seq.md" class="link" title="Chapter 11.  The DB_SEQUENCE Handle">DB_SEQUENCE</a> handle itself be transactionally protected during its open if the open creates the sequence.

### Class

<a href="seq.md" class="link" title="Chapter 11.  The DB_SEQUENCE Handle">DB_SEQUENCE</a>

### See Also

<a href="seq.md#seqlist" class="xref" title="Sequences and Related Methods">Sequences and Related Methods</a>
