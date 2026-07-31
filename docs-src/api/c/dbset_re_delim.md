---
title: "DB->set_re_delim()"
api-name: "DB->set_re_delim()"
source: docs/api_reference/C/dbset_re_delim.html
---
## DB-\>set_re_delim()

``` c
#include <db.h>

int
DB->set_re_delim(DB *db, int *re_delim);  
```

Set the delimiting byte used to mark the end of a record in the backing source file for the Recno access method.

This byte is used for variable length records if the **re_source** file is specified using the <a href="dbset_re_source.md" class="xref" title="DB-&gt;set_re_source()">DB-&gt;set_re_source()</a> method. If the **re_source** file is specified and no delimiting byte was specified, \<newline\> characters (that is, ASCII 0x0a) are interpreted as end-of-record markers.

The `DB->set_re_delim()` method configures a database, not only operations performed using the specified <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle.

The `DB->set_re_delim()` method may not be called after the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called. If the database already exists when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called, the information specified to `DB->set_re_delim()` will be ignored.

The `DB->set_re_delim()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### re_delim

The **re_delim** parameter is the delimiting byte used to mark the end of a record.

### Errors

The `DB->set_re_delim()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
