---
title: "DB->set_h_nelem()"
api-name: "DB->set_h_nelem()"
source: docs/api_reference/C/dbset_h_nelem.html
---
## DB-\>set_h_nelem()

``` c
#include <db.h>

int
DB->set_h_nelem(DB *db, u_int32_t h_nelem);  
```

Set an estimate of the final size of the hash table.

In order for the estimate to be used when creating the database, the <a href="dbset_h_ffactor.md" class="xref" title="DB-&gt;set_h_ffactor()">DB-&gt;set_h_ffactor()</a> method must also be called. If the estimate or fill factor are not set or are set too low, hash tables will still expand gracefully as keys are entered, although a slight performance degradation may be noticed.

The `DB->set_h_nelem()` method configures a database, not only operations performed using the specified <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle.

The `DB->set_h_nelem()` method may not be called after the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called. If the database already exists when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called, the information specified to `DB->set_h_nelem()` will be ignored.

The `DB->set_h_nelem()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### h_nelem

The **h_nelem** parameter is an estimate of the final size of the hash table.

### Errors

The `DB->set_h_nelem()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
