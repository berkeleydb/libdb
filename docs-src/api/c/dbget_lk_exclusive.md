---
title: "DB->get_lk_exclusive()"
api-name: "DB->get_lk_exclusive()"
source: docs/api_reference/C/dbget_lk_exclusive.html
---
## DB-\>get_lk_exclusive()

``` c
#include <db.h>

int
DB->get_lk_exclusive(DB *db, int *onoff, int *nowait);  
```

Returns whether the database handle is configured to obtain a write lock on the entire database. This can be set using the <a href="dbset_lk_exclusive.md" class="xref" title="DB-&gt;set_lk_exclusive()">DB-&gt;set_lk_exclusive()</a> method.

The `DB->get_lk_exclusive()` method may be called at any time during the life of the application.

The `DB->get_lk_exclusive()` always returns `0`.

### Parameters

#### onoff

Indicates whether the handle is configured for exclusive database locking. If `0`, it is not configured for exclusive locking. If `1`, then it is configured for exclusive locking.

#### nowait

Indicates whether the handle is configured for immediate locking. If `0`, then the locking operation will block until it can obtain an exclusive database lock. If `1`, then the locking operation will error out if it cannot immediately obtain an exclusive lock.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>, <a href="dbset_lk_exclusive.md" class="xref" title="DB-&gt;set_lk_exclusive()">DB-&gt;set_lk_exclusive()</a>
