---
title: "DB->get_bt_minkey()"
api-name: "DB->get_bt_minkey()"
source: docs/api_reference/C/dbget_bt_minkey.html
---
## DB-\>get_bt_minkey()

``` c
#include <db.h>

int
DB->get_bt_minkey(DB *db, u_int32_t *bt_minkeyp);  
```

The `DB->get_bt_minkey()` method returns the minimum number of key/data pairs intended to be stored on any single Btree leaf page. This value can be set using the <a href="dbset_bt_minkey.md" class="xref" title="DB-&gt;set_bt_minkey()">DB-&gt;set_bt_minkey()</a> method.

The `DB->get_bt_minkey()` method may be called at any time during the life of the application.

The `DB->get_bt_minkey()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### bt_minkeyp

The `DB->get_bt_minkey()` method returns the minimum number of key/data pairs intended to be stored on any single Btree leaf page in **bt_minkeyp**.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>, <a href="dbset_bt_minkey.md" class="xref" title="DB-&gt;set_bt_minkey()">DB-&gt;set_bt_minkey()</a>
