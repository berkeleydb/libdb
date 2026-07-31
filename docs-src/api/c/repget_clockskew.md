---
title: "DB_ENV->rep_get_clockskew()"
api-name: "DB_ENV->rep_get_clockskew()"
source: docs/api_reference/C/repget_clockskew.html
---
## DB_ENV-\>rep_get_clockskew()

``` c
#include <db.h>

int
DB_ENV->rep_get_clockskew(DB_ENV *env,
    u_int32_t *fast_clockp, u_int32_t *slow_clockp);  
```

The `DB_ENV->rep_get_clockskew()` method returns the current clock skew ratio values, as set by the <a href="repclockskew.md" class="xref" title="DB_ENV-&gt;rep_set_clockskew()">DB_ENV-&gt;rep_set_clockskew()</a> method.

The `DB_ENV->rep_get_clockskew()` method may be called at any time during the life of the application.

The `DB_ENV->rep_get_clockskew()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### fast_clockp

The **fast_clockp** parameter references memory into which the value for the fastest clock in the group of sites is copied.

#### slow_clockp

The **slow_clockp** parameter references memory into which the value for the slowest clock in the group of sites is copied.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>, <a href="repclockskew.md" class="xref" title="DB_ENV-&gt;rep_set_clockskew()">DB_ENV-&gt;rep_set_clockskew()</a>
