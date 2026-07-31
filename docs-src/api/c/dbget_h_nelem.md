---
title: "DB->get_h_nelem()"
api-name: "DB->get_h_nelem()"
source: docs/api_reference/C/dbget_h_nelem.html
---
## DB-\>get_h_nelem()

``` c
#include <db.h>

int
DB->get_h_nelem(DB *db, u_int32_t *h_nelemp);  
```

The `DB->get_h_nelem()` method returns the estimate of the final size of the hash table as set by the <a href="dbset_h_nelem.md" class="xref" title="DB-&gt;set_h_nelem()">DB-&gt;set_h_nelem()</a> method.

The `DB->get_h_nelem()` method may be called at any time during the life of the application.

The `DB->get_h_nelem()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### h_nelemp

The `DB->get_h_nelem()` method returns the estimate of the final size of the hash table in **h_nelemp**.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>, <a href="dbset_h_nelem.md" class="xref" title="DB-&gt;set_h_nelem()">DB-&gt;set_h_nelem()</a>
