---
title: "DB->set_bt_minkey()"
api-name: "DB->set_bt_minkey()"
source: docs/api_reference/C/dbset_bt_minkey.html
---
## DB-\>set_bt_minkey()

``` c
#include <db.h>

int
DB->set_bt_minkey(DB *db, u_int32_t bt_minkey);  
```

Set the minimum number of key/data pairs intended to be stored on any single Btree leaf page.

This value is used to determine if key or data items will be stored on overflow pages instead of Btree leaf pages. For more information on the specific algorithm used, see <a href="../../programmer_reference/bt_conf.html#am_conf_bt_minkey" class="olink">Minimum keys per page</a>. The **bt_minkey** value specified must be at least 2; if **bt_minkey** is not explicitly set, a value of 2 is used.

The `DB->set_bt_minkey()` method configures a database, not only operations performed using the specified <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle.

The `DB->set_bt_minkey()` method may not be called after the <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> method is called. If the database already exists when <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> is called, the information specified to `DB->set_bt_minkey()` will be ignored.

The `DB->set_bt_minkey()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### bt_minkey

The **bt_minkey** parameter is the minimum number of key/data pairs intended to be stored on any single Btree leaf page.

### Errors

The `DB->set_bt_minkey()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called after <a href="dbopen.md" class="xref" title="DB-&gt;open()">DB-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
