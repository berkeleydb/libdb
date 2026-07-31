---
title: "DB_SEQUENCE->remove()"
api-name: "DB_SEQUENCE->remove()"
source: docs/api_reference/C/seqremove.html
---
## DB_SEQUENCE-\>remove()

``` c
#include <db.h>

int
DB_SEQUENCE->remove(DB_SEQUENCE *seq, DB_TXN *txnid, u_int32_t flags);  
```

The `DB_SEQUENCE->remove()` method removes the sequence from the database. This method should not be called if there are other open handles on this sequence.

The <a href="seq.md" class="link" title="Chapter 11.  The DB_SEQUENCE Handle">DB_SEQUENCE</a> handle may not be accessed again after `DB_SEQUENCE->remove()` is called, regardless of its return.

The `DB_SEQUENCE->remove()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### txnid

If the operation is part of an application-specified transaction, the **txnid** parameter is a transaction handle returned from <a href="txnbegin.md" class="xref" title="DB_ENV-&gt;txn_begin()">DB_ENV-&gt;txn_begin()</a>; if the operation is part of a Berkeley DB Concurrent Data Store group, the **txnid** parameter is a handle returned from <a href="envcdsgroup_begin.md" class="xref" title="DB_ENV-&gt;cdsgroup_begin()">DB_ENV-&gt;cdsgroup_begin()</a>; otherwise NULL. If no transaction handle is specified, but the operation occurs in a transactional database, the operation will be implicitly transaction protected.

#### flags

The **flags** parameter must be set to 0 or by bitwise inclusively **OR**'ing together one or more of the following values:

- `DB_TXN_NOSYNC`

  If the operation is implicitly transaction protected (the **txnid** argument is NULL but the operation occurs to a transactional database), do not synchronously flush the log when the transaction commits.

### Errors

The `DB_SEQUENCE->remove()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="seq.md" class="link" title="Chapter 11.  The DB_SEQUENCE Handle">DB_SEQUENCE</a>

### See Also

<a href="seq.md#seqlist" class="xref" title="Sequences and Related Methods">Sequences and Related Methods</a>
