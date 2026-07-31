---
title: "DB->set_re_len()"
api-name: "DB->set_re_len()"
source: docs/api_reference/C/dbset_re_len.html
---
## DB-\>set_re_len()

``` c
#include <db.h>

int
DB->set_re_len(DB *db, u_int32_t re_len);  
```

For the Queue access method, specify that the records are of length **re_len**. For the Queue access method, the record length must be enough smaller than the database's page size that at least one record plus the database page's metadata information can fit on each database page.

For the Recno access method, specify that the records are fixed-length, not byte-delimited, and are of length **re_len**.

Any records added to the database that are less than **re_len** bytes long are automatically padded (see <a href="dbset_re_pad.md" class="xref" title="DB-&gt;set_re_pad()">DB-&gt;set_re_pad()</a> for more information).

Any attempt to insert records into the database that are greater than **re_len** bytes long will cause the call to fail immediately and return an error.

The `DB->set_re_len()` method configures a database, not only operations performed using the specified <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle.

The `DB->set_re_len()` method may not be called after the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called. If the database already exists when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called, the information specified to `DB->set_re_len()` will be ignored.

The `DB->set_re_len()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### re_len

The **re_len** parameter is the length of a Queue or Recno database record, in bytes.

### Errors

The `DB->set_re_len()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
