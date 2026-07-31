---
title: "DB->get_priority()"
api-name: "DB->get_priority()"
source: docs/api_reference/C/dbget_priority.html
---
## DB-\>get_priority()

``` c
#include <db.h>

int
DB->get_priority(DB *db, DB_CACHE_PRIORITY *priorityp);  
```

The `DB->get_priority()` method returns the cache priority for pages referenced by the <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle. This priority value is set using the <a href="dbset_priority.md" class="xref" title="DB-&gt;set_priority()">DB-&gt;set_priority()</a> method.

The `DB->get_priority()` method may be called only after the database has been opened.

The `DB->get_priority()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### priorityp

The `DB->get_priority()` method returns a reference to the cache priority in **priorityp**. See <a href="dbset_priority.md" class="xref" title="DB-&gt;set_priority()">DB-&gt;set_priority()</a> for a list of possible priorities.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>, <a href="dbset_priority.md" class="xref" title="DB-&gt;set_priority()">DB-&gt;set_priority()</a>
