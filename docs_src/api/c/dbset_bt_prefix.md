---
title: "DB->set_bt_prefix()"
api-name: "DB->set_bt_prefix()"
source: docs/api_reference/C/dbset_bt_prefix.html
---
## DB-\>set_bt_prefix()

``` c
#include <db.h>

int
DB->set_bt_prefix(DB *db,
    size_t (*bt_prefix_fcn)(DB *, const *dbt1, const *dbt2));  
```

Set the Btree prefix function. The prefix function is used to determine the amount by which keys stored on the Btree internal pages can be safely truncated without losing their uniqueness. See the <a href="../../guides/programmer_reference/bt_conf.md#am_conf_bt_prefix" class="olink">Btree prefix comparison</a> section of the Berkeley DB Reference Guide for more details about how this works. The usefulness of this is data-dependent, but can produce significantly reduced tree sizes and search times in some data sets.

If no prefix function or key comparison function is specified by the application, a default lexical comparison function is used as the prefix function. If no prefix function is specified and a key comparison function is specified, no prefix function is used. It is an error to specify a prefix function without also specifying a Btree key comparison function.

The `DB->set_bt_prefix()` method configures operations performed using the specified <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle, not all operations performed on the underlying database.

The `DB->set_bt_prefix()` method may not be called after the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called. If the database already exists when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called, the information specified to `DB->set_bt_prefix()` must be the same as that historically used to create the database or corruption can occur.

The `DB->set_bt_prefix()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### bt_prefix_fcn

The **bt_prefix_fcn** function is the application-specific Btree prefix function. The prefix function takes three parameters:

- `db`

  The **db** parameter is the enclosing database handle.

- `dbt1`

  The **dbt1** parameter is a <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> representing a database key.

- `dbt2`

  The **dbt2** parameter is a <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> representing a database key.

The **bt_prefix_fcn** function must return the number of bytes of the second key parameter that would be required by the Btree key comparison function to determine the second key parameter's ordering relationship with respect to the first key parameter. If the two keys are equal, the key length should be returned. The prefix function must correctly handle any key values used by the application (possibly including zero-length keys). The **data** and **size** fields of the <a href="dbt.md" class="link" title="Chapter 4.  The DBT Handle">DBT</a> are the only fields that may be used for the purposes of this determination, and no particular alignment of the memory to which the **data** field refers may be assumed.

### Page bytes passed to this callback

By default this callback is invoked only on a latched, pinned page, so its page
bytes are a self-consistent snapshot of one committed page state.

Setting `DB_OPTREAD` in the environment enables the optimistic (pin-free)
interior descent, under which application callbacks reached from a page descent
may be called on **unvalidated** page bytes: libdb guarantees the bytes lie
inside the page frame, but not that they are consistent, and the content may be
any byte sequence that happened to be in the frame. Such a callback must then be
total over arbitrary bytes — it must terminate and must not fault for any
content or any size.

`DB_OPTREAD` is **off by default** for exactly this reason. The full statement
of what is and is not guaranteed, and the list of callback shapes that are
unsafe under it, is in
<a href="dbset_bt_compare.md" class="xref" title="DB-&gt;set_bt_compare()">DB-&gt;set_bt_compare()</a>.

### Errors

The `DB->set_bt_prefix()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
