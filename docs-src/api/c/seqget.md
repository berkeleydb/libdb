---
title: "DB_SEQUENCE->get()"
api-name: "DB_SEQUENCE->get()"
source: docs/api_reference/C/seqget.html
---
## DB_SEQUENCE-\>get()

``` c
#include <db.h>

int
DB_SEQUENCE->get(DB_SEQUENCE *seq,
    DB_TXN *txnid, int32_t delta, db_seq_t *retp, u_int32_t flags);  
```

The `DB_SEQUENCE->get()` method returns the next available element in the sequence and changes the sequence value by **delta**. The value of **delta** must be greater than zero. If there are enough cached values in the sequence handle then they will be returned. Otherwise the next value will be fetched from the database and incremented (decremented) by enough to cover the **delta** and the next batch of cached values.

For maximum concurrency a non-zero cache size should be specified prior to opening the sequence handle and <a href="envset_flags.md#envset_flags_DB_TXN_NOSYNC" class="link">DB_TXN_NOSYNC</a> should be specified for each `DB_SEQUENCE->get()` method call.

By default, sequence ranges do not wrap; to cause the sequence to wrap around the beginning or end of its range, specify the <a href="seqset_flags.md#seqset_flags_DB_SEQ_WRAP" class="link">DB_SEQ_WRAP</a> flag to the <a href="seqset_flags.md" class="xref" title="DB_SEQUENCE-&gt;set_flags()">DB_SEQUENCE-&gt;set_flags()</a> method.

The `DB_SEQUENCE->get()` method will return EINVAL if the record in the database is not a valid sequence record, or the sequence has reached the beginning or end of its range and is not configured to wrap.

### Parameters

#### txnid

If the operation is part of an application-specified transaction, the **txnid** parameter is a transaction handle returned from <a href="txnbegin.md" class="xref" title="DB_ENV-&gt;txn_begin()">DB_ENV-&gt;txn_begin()</a>; if the operation is part of a Berkeley DB Concurrent Data Store group, the **txnid** parameter is a handle returned from <a href="envcdsgroup_begin.md" class="xref" title="DB_ENV-&gt;cdsgroup_begin()">DB_ENV-&gt;cdsgroup_begin()</a>; otherwise NULL. If no transaction handle is specified, but the operation occurs in a transactional database, the operation will be implicitly transaction protected. No **txnid** handle may be specified if the sequence handle was opened with a non-zero cache size.

If the underlying database handle was opened in a transaction, calling `DB_SEQUENCE->get()` may result in changes to the sequence object; these changes will be automatically committed in a transaction internal to the Berkeley DB library. If the thread of control calling `DB_SEQUENCE->get()` has an active transaction, which holds locks on the same database as the one in which the sequence object is stored, it is possible for a thread of control calling `DB_SEQUENCE->get()` to self-deadlock because the active transaction's locks conflict with the internal transaction's locks. For this reason, it is often preferable for sequence objects to be stored in their own database.

#### delta

Specifies the amount to increment or decrement the sequence.

#### retp

**retp** points to the memory to hold the return value from the sequence.

#### flags

The **flags** parameter must be set to 0 or by bitwise inclusively **OR**'ing together one or more of the following values:

- `DB_TXN_NOSYNC`

  If the operation is implicitly transaction protected (the **txnid** argument is NULL but the operation occurs to a transactional database), do not synchronously flush the log when the transaction commits.

### Class

<a href="seq.md" class="link" title="Chapter 11.  The DB_SEQUENCE Handle">DB_SEQUENCE</a>

### See Also

<a href="seq.md#seqlist" class="xref" title="Sequences and Related Methods">Sequences and Related Methods</a>
