---
title: "db_get_multiple()"
api-name: "db_get_multiple()"
---
## db_get_multiple()

``` c
#include <db.h>

int
db_get_multiple(DB *db, DB_TXN *txnid,
    DBT *keys, DBT *datas, int *rets, u_int32_t nkeys, u_int32_t flags);
```

The `db_get_multiple()` function retrieves **many scattered keys in a single API
crossing**. It is semantically identical to calling
<a href="dbget.md" class="xref" title="DB-&gt;get()">DB-&gt;get()</a> once per key:
each key is looked up by its own B-tree descent, takes its own page read lock
(or its own SIREAD marker under `DB_TXN_SERIALIZABLE`), and its result is placed
in `datas[i]` exactly as the individual call would have returned it.

What the function amortizes is only the *per-call* overhead — one environment
entry/exit, one replication check, one transaction-consistency check, and above
all **one cursor allocate/free pair instead of `nkeys`**. On a machine with many
cores that cursor lifecycle, not the tree descent, dominates a concurrent point-read
workload, so batching it is where the time is. See
[`BATCHED-READS-RESULTS.md`](https://github.com/berkeleydb/libdb/blob/master/test/bench/BATCHED-READS-RESULTS.md)
for the measurement.

`db_get_multiple()` is a **free function rather than a `DB` method** by design:
adding a method pointer to the `DB` handle would change both `sizeof(DB)` (the
public ABI) and the environment build signature, and the latter would make
existing environments refuse to attach.

## Parameters

**keys**  
An array of `nkeys` `DBT`s holding the keys to retrieve.

**datas**  
An array of `nkeys` `DBT`s that receive the retrieved data, one per key, using
the same `DBT` flags and semantics as
<a href="dbget.md" class="xref" title="DB-&gt;get()">DB-&gt;get()</a>.

**rets**  
An array of `nkeys` `int`s that receives each key's *individual* return code —
`0`, `DB_NOTFOUND`, `DB_BUFFER_SMALL`, and so on. May be `NULL` if the caller
does not need per-key codes.

**nkeys**  
The number of keys. If `nkeys` is 0 the function returns 0 immediately.

**txnid**  
If the operation is part of a transaction, the transaction handle returned by
<a href="txnbegin.md" class="xref" title="DB_ENV-&gt;txn_begin()">DB_ENV-&gt;txn_begin()</a>;
otherwise `NULL`. The same transaction (and therefore the same isolation level)
applies to every key in the batch.

**flags**  
Only the read flags that are meaningful per key are accepted; they apply to every
key in the batch.

## Return value

`db_get_multiple()` returns 0 if every key either succeeded or was simply not
found. Otherwise it returns the **first** per-key code that is neither 0 nor
`DB_NOTFOUND`, and lookups stop at that key — exactly where a caller looping over
<a href="dbget.md" class="xref" title="DB-&gt;get()">DB-&gt;get()</a> and breaking on
error would have stopped. Inspect `rets` to see which keys were resolved.

## Errors

`EINVAL`  
An invalid flag value or parameter was specified.

Otherwise, the per-key errors documented for
<a href="dbget.md" class="xref" title="DB-&gt;get()">DB-&gt;get()</a> are reported
through `rets`.

## Isolation

Because each key is looked up exactly as an individual
<a href="dbget.md" class="xref" title="DB-&gt;get()">DB-&gt;get()</a> would be, the
batch has the *same* isolation behaviour as the equivalent loop, including
conflict detection and abort behaviour under `DB_TXN_SERIALIZABLE`: the same read
set is recorded, so the same serialization conflicts are detected. Batching
changes performance, never semantics.

## Class

[DB](db.md)

## See also

<a href="dbget.md" class="xref" title="DB-&gt;get()">DB-&gt;get()</a>,
[Database and related methods](db.md)
