---
title: "DB->set_bt_compare()"
api-name: "DB->set_bt_compare()"
source: docs/api_reference/C/dbset_bt_compare.html
---
## DB-\>set_bt_compare()

``` c
#include <db.h>

int
DB->set_bt_compare(DB *db,
    int (*bt_compare_fcn)(DB *db, const DBT *dbt1, const DBT *dbt2));  
```

Set the Btree key comparison function. The comparison function is called whenever it is necessary to compare a key specified by the application with a key currently stored in the tree.

If no comparison function is specified, the keys are compared lexically, with shorter keys collating before longer keys.

The `DB->set_bt_compare()` method configures operations performed using the specified <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle, not all operations performed on the underlying database.

The `DB->set_bt_compare()` method may not be called after the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called. If the database already exists when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called, the information specified to `DB->set_bt_compare()` must be the same as that historically used to create the database or corruption can occur.

The `DB->set_bt_compare()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### bt_compare_fcn

The **bt_compare_fcn** function is the application-specified Btree comparison function. The comparison function takes three parameters:

- `db`

  The **db** parameter is the enclosing database handle.

- `dbt1`

  The **dbt1** parameter is the <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> representing the application supplied key.

- `dbt2`

  The **dbt2** parameter is the <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> representing the current tree's key.

The **bt_compare_fcn** function must return an integer value less than, equal to, or greater than zero if the first key parameter is considered to be respectively less than, equal to, or greater than the second key parameter. In addition, the comparison function must cause the keys in the database to be <span class="emphasis">*well-ordered*</span>. The comparison function must correctly handle any key values used by the application (possibly including zero-length keys). In addition, when Btree key prefix comparison is being performed (see <a href="dbset_bt_prefix.md" class="xref" title="DB-&gt;set_bt_prefix()">DB-&gt;set_bt_prefix()</a> for more information), the comparison routine may be passed a prefix of any database key. The **data** and **size** fields of the <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> are the only fields that may be used for the purposes of this comparison, and no particular alignment of the memory to which by the **data** field refers may be assumed.

### Page bytes passed to the comparison function

By default, `bt_compare_fcn` is called only on a page that libdb holds latched
and pinned. The `dbt2` bytes are a self-consistent snapshot of one committed
page state, and that is the contract you may rely on.

**`DB_OPTREAD` weakens this contract.** It enables the optimistic (pin-free)
interior descent described in RFC 0007, which walks interior pages with no pin
and no latch and validates afterwards. Under `DB_OPTREAD`, and **only** under
it, your comparison function may be called on *unvalidated* page bytes. Exactly
what remains guaranteed, and what does not:

**Still guaranteed.** `dbt2.data` and the `dbt2.size` bytes following it lie
inside the page frame. libdb will not fault, will not follow a pointer out of
the page, and will not write to it. A comparison performed on torn bytes
produces a wrong child, which validation then detects and discards; the query
still returns the correct answer.

**No longer guaranteed.** The bytes may be a *torn mix of two different page
states*. `dbt2.size` may describe one record while `dbt2.data` points into
another. The content may be any byte sequence that happened to be in the frame,
including sequences that never existed as a key in the database.

Therefore, under `DB_OPTREAD` the comparison function **must be total over
arbitrary bytes**: it must terminate, and it must not fault, abort, or raise,
for *any* `dbt2` content and *any* `dbt2.size`. This is a stronger requirement
than the usual one. A comparator is **not** safe here if it:

- asserts, or calls `abort()`, on input it considers malformed;
- reads a length, count, type tag, or offset out of the key bytes and then uses
  it to index, loop, or dereference;
- requires a terminator (such as a trailing `NUL`) to stop scanning;
- indexes a lookup or collation table by a value decoded from the key, without
  bounds-checking that value;
- dereferences a pointer stored inside the key.

A comparator that only compares bytes — `memcmp`, a fixed-width integer or
fixed-layout struct comparison, or any function whose reads are bounded by
`dbt2.size` alone — is safe.

If you cannot state with certainty that your comparison function is total over
arbitrary bytes, **do not set `DB_OPTREAD`.** The default is off precisely
because this is a property libdb cannot verify for you, and the failure mode is
a crash inside your own code on a page race, which is difficult to reproduce and
whose stack will not obviously point here.

The feature is disabled by default and is enabled only by setting `DB_OPTREAD`
in the environment. `DB_NO_OPTREAD` forces it off and takes precedence, so a
script may disable it unconditionally without inspecting the rest of the
environment. The reported benefit is a 1.71x improvement in per-key read
throughput at 32 threads and 2.05x on the batched API at 96 threads; see
`test/bench/OPTIMISTIC-READS-2026-09.md` for the measurements and
`rfc/0007-optimistic-read-validation.md` for the design and its open risks.

Note that this affects the Btree comparison, prefix, duplicate-comparison and
compression callbacks equally: any application function reached from an interior
page descent is subject to the same weakened guarantee.

### Errors

The `DB->set_bt_compare()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
