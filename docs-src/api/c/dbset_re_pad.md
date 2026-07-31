---
title: "DB->set_re_pad()"
api-name: "DB->set_re_pad()"
source: docs/api_reference/C/dbset_re_pad.html
---
## DB-\>set_re_pad()

``` c
#include <db.h>

int
DB->set_re_pad(DB *db, int re_pad);  
```

Set the padding character for short, fixed-length records for the Queue and Recno access methods.

If no pad character is specified, \<space\> characters (that is, ASCII 0x20) are used for padding.

The `DB->set_re_pad()` method configures a database, not only operations performed using the specified <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle.

The `DB->set_re_pad()` method may not be called after the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called. If the database already exists when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called, the information specified to `DB->set_re_pad()` will be ignored.

The `DB->set_re_pad()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### re_pad

The **re_pad** parameter is the pad character for fixed-length records for the Queue and Recno access methods.

### Errors

The `DB->set_re_pad()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
