---
title: "DB_ENV->rep_get_priority()"
api-name: "DB_ENV->rep_get_priority()"
source: docs/api_reference/C/repget_priority.html
---
## DB_ENV-\>rep_get_priority()

``` c
#include <db.h>

int
DB_ENV->rep_get_priority(DB_ENV *env, u_int32_t *priorityp);  
```

The `DB_ENV->rep_get_priority()` method returns the database environment priority as configured using the <a href="reppriority.md" class="xref" title="DB_ENV-&gt;rep_set_priority()">DB_ENV-&gt;rep_set_priority()</a> method.

The `DB_ENV->rep_get_priority()` method may be called at any time during the life of the application.

The `DB_ENV->rep_get_priority()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### priorityp

The `DB_ENV->rep_get_priority()` method returns the database environment priority in **priorityp**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>, <a href="reppriority.md" class="xref" title="DB_ENV-&gt;rep_set_priority()">DB_ENV-&gt;rep_set_priority()</a>
