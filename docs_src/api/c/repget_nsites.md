---
title: "DB_ENV->rep_get_nsites()"
api-name: "DB_ENV->rep_get_nsites()"
source: docs/api_reference/C/repget_nsites.html
---
## DB_ENV-\>rep_get_nsites()

``` c
#include <db.h>

int
DB_ENV->rep_get_nsites(DB_ENV *env, u_int32_t *nsitesp);  
```

The `DB_ENV->rep_get_nsites()` method returns the total number of sites in the replication group. For Base API applications, his value is configurable using the <a href="repnsites.md" class="xref" title="DB_ENV-&gt;rep_set_nsites()">DB_ENV-&gt;rep_set_nsites()</a> method. For Replication Manager applications, this value is determined dynamically.

For Base API applications, this method may be called at any time during the life of the application. For Replication Manager applications, this method may be called only after a successful call to the <a href="repmgrstart.md" class="xref" title="DB_ENV-&gt;repmgr_start()">DB_ENV-&gt;repmgr_start()</a> method.

The `DB_ENV->rep_get_nsites()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### nsitesp

The `DB_ENV->rep_get_nsites()` method returns the total number of sites in the replication group in **nsitesp**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>, <a href="repnsites.md" class="xref" title="DB_ENV-&gt;rep_set_nsites()">DB_ENV-&gt;rep_set_nsites()</a>
