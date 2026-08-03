---
title: "DB->set_priority()"
api-name: "DB->set_priority()"
source: docs/api_reference/C/dbset_priority.html
---
## DB-\>set_priority()

``` c
#include <db.h>

int
DB->set_priority(DB *db, DB_CACHE_PRIORITY priority);  
```

Set the cache priority for pages referenced by the <a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a> handle.

The priority of a page biases the replacement algorithm to be more or less likely to discard a page when space is needed in the buffer pool. The bias is temporary, and pages will eventually be discarded if they are not referenced again. The `DB->set_priority()` method is only advisory, and does not guarantee pages will be treated in a specific way.

The `DB->set_priority()` method may be called at any time during the life of the application.

The `DB->set_priority()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### priority

The **priority** parameter must be set to one of the following values:

- `DB_PRIORITY_VERY_LOW`

  The lowest priority: pages are the most likely to be discarded.

- `DB_PRIORITY_LOW`

  The next lowest priority.

- `DB_PRIORITY_DEFAULT`

  The default priority.

- `DB_PRIORITY_HIGH`

  The next highest priority.

- `DB_PRIORITY_VERY_HIGH`

  The highest priority: pages are the least likely to be discarded.

### Class

<a href="db.md" class="link" title="Chapter 2.  The DB Handle">DB</a>

### See Also

<a href="db.md#dblist" class="xref" title="Database and Related Methods">Database and Related Methods</a>
