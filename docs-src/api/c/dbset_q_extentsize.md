---
title: "DB->set_q_extentsize()"
api-name: "DB->set_q_extentsize()"
source: docs/api_reference/C/dbset_q_extentsize.html
---
## DB-\>set_q_extentsize()

``` c
#include <db.h>

int
DB->set_q_extentsize(DB *db, u_int32_t extentsize);  
```

Set the size of the extents used to hold pages in a Queue database, specified as a number of pages. Each extent is created as a separate physical file. If no extent size is set, the default behavior is to create only a single underlying database file.

For information on tuning the extent size, see <a href="../../guides/programmer_reference/rq_conf.md#am_conf_extentsize" class="olink">Selecting a extent size</a>.

The `DB->set_q_extentsize()` method configures a database, not only operations performed using the specified <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle.

The `DB->set_q_extentsize()` method may not be called after the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called. If the database already exists when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called, the information specified to `DB->set_q_extentsize()` will be ignored.

The `DB->set_q_extentsize()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### extentsize

The **extentsize** parameter is the number of pages in a Queue database extent.

### Errors

The `DB->set_q_extentsize()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
