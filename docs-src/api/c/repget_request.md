---
title: "DB_ENV->rep_get_request()"
api-name: "DB_ENV->rep_get_request()"
source: docs/api_reference/C/repget_request.html
---
## DB_ENV-\>rep_get_request()

``` c
#include <db.h>

int
DB_ENV->rep_get_request(DB_ENV *env, u_int32_t *minp, u_int32_t *maxp);  
```

The `DB_ENV->rep_get_request()` method returns the minimum and maximum number of microseconds a client waits before requesting retransmission. These values can be configured using the <a href="repset_request.md" class="xref" title="DB_ENV-&gt;rep_set_request()">DB_ENV-&gt;rep_set_request()</a> method.

The `DB_ENV->rep_get_request()` method may be called at any time during the life of the application.

The `DB_ENV->rep_get_request()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### minp

The **minp** parameter references memory into which the minimum number of microseconds a client will wait before requesting retransmission is copied.

#### maxp

The **maxp** parameter references memory into which the maximum number of microseconds a client will wait before requesting retransmission is copied.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>, <a href="repset_request.md" class="xref" title="DB_ENV-&gt;rep_set_request()">DB_ENV-&gt;rep_set_request()</a>
