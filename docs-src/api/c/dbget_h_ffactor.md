---
title: "DB->get_h_ffactor()"
api-name: "DB->get_h_ffactor()"
source: docs/api_reference/C/dbget_h_ffactor.html
---
## DB-\>get_h_ffactor()

``` c
#include <db.h>

int
DB->get_h_ffactor(DB *db, u_int32_t *h_ffactorp);  
```

The `DB->get_h_ffactor()` method returns the hash table density as set by the <a href="dbset_h_ffactor.md" class="xref" title="DB-&gt;set_h_ffactor()">DB-&gt;set_h_ffactor()</a> method. The hash table density is the number of items that Berkeley DB tries to place in a hash bucket before splitting the hash bucket.

The `DB->get_h_ffactor()` method may be called at any time during the life of the application.

The `DB->get_h_ffactor()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### h_ffactorp

The `DB->get_h_ffactor()` method returns the hash table density in **h_ffactorp**.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>, <a href="dbset_h_ffactor.md" class="xref" title="DB-&gt;set_h_ffactor()">DB-&gt;set_h_ffactor()</a>
