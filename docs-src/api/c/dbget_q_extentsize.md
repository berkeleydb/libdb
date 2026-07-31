---
title: "DB->get_q_extentsize()"
api-name: "DB->get_q_extentsize()"
source: docs/api_reference/C/dbget_q_extentsize.html
---
## DB-\>get_q_extentsize()

``` c
#include <db.h>

int
DB->get_q_extentsize(DB *db, u_int32_t *extentsizep);  
```

The `DB->get_q_extentsize()` method returns the number of pages in an extent. This value is used only for Queue databases and is set using the <a href="dbset_q_extentsize.md" class="xref" title="DB-&gt;set_q_extentsize()">DB-&gt;set_q_extentsize()</a> method.

The `DB->get_q_extentsize()` method may be called only after the database has been opened.

The `DB->get_q_extentsize()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### extentsizep

The `DB->get_q_extentsize()` method returns the number of pages in an extent in **extentsizep**. If used on a handle that has not yet been opened, `0` is returned.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>, <a href="dbset_q_extentsize.md" class="xref" title="DB-&gt;set_q_extentsize()">DB-&gt;set_q_extentsize()</a>
