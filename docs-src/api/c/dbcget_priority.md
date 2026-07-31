---
title: "DBcursor->get_priority()"
api-name: "DBcursor->get_priority()"
source: docs/api_reference/C/dbcget_priority.html
---
## DBcursor-\>get_priority()

``` c
#include <db.h>

int
DBcursor->get_priority(DBC *DbCursor, DB_CACHE_PRIORITY *priorityp);  
```

The `DBcursor->get_priority()` method returns the cache priority for pages referenced by the <a href="dbc.md" class="link" title="Chapter 3.  The DBcursor Handle">DBcursor</a> handle.

The `DBcursor->get_priority()` method may be called at any time during the life of the application.

The `DBcursor->get_priority()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### priorityp

The `DBcursor->get_priority()` method returns a reference to the cache priority for pages referenced by the <a href="dbc.md" class="link" title="Chapter 3.  The DBcursor Handle">DBcursor</a> handle in **priorityp**.

### Class

<a href="dbc.md" class="link" title="Chapter 3.  The DBcursor Handle">DBcursor</a>

### See Also

<a href="dbc.md#dbclist" class="xref" title="Database Cursors and Related Methods">Database Cursors and Related Methods</a>
