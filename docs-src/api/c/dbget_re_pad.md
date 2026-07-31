---
title: "DB->get_re_pad()"
api-name: "DB->get_re_pad()"
source: docs/api_reference/C/dbget_re_pad.html
---
## DB-\>get_re_pad()

``` c
#include <db.h>

int
DB->get_re_pad(DB *db, int *re_padp);  
```

The `DB->get_re_pad()` method returns the pad character used for short, fixed-length records used by the Queue and Recno access methods. This character is set using the <a href="dbset_re_pad.md" class="xref" title="DB-&gt;set_re_pad()">DB-&gt;set_re_pad()</a> method.

The `DB->get_re_pad()` method may be called only after the database has been opened.

The `DB->get_re_pad()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### re_padp

The `DB->get_re_pad()` method returns the pad character in **re_padp**. If used on a handle that has not yet been opened, the default pad character is returned. See the <a href="dbset_re_pad.md" class="xref" title="DB-&gt;set_re_pad()">DB-&gt;set_re_pad()</a> method description for what that default value is.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>, <a href="dbset_re_pad.md" class="xref" title="DB-&gt;set_re_pad()">DB-&gt;set_re_pad()</a>
