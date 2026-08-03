---
title: "DB_ENV->rep_get_limit()"
api-name: "DB_ENV->rep_get_limit()"
source: docs/api_reference/C/repget_limit.html
---
## DB_ENV-\>rep_get_limit()

``` c
#include <db.h>

int
DB_ENV->rep_get_limit(DB_ENV *env, u_int32_t *gbytesp, 
    u_int32_t *bytesp);  
```

The `DB_ENV->rep_get_limit()` method returns the byte-count limit on the amount of data that will be transmitted from a site in response to a single message processed by the <a href="repmessage.md" class="xref" title="DB_ENV-&gt;rep_process_message()">DB_ENV-&gt;rep_process_message()</a> method. This value is configurable using the <a href="repset_limit.md" class="xref" title="DB_ENV-&gt;rep_set_limit()">DB_ENV-&gt;rep_set_limit()</a> method.

The `DB_ENV->rep_get_limit()` method may be called at any time during the life of the application.

The `DB_ENV->rep_get_limit()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### gbytesp

The **gbytesp** parameter references memory into which the gigabytes component of the current transmission limit is copied.

#### bytesp

The **bytesp** parameter references memory into which the bytes component of the current transmission limit is copied.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>, <a href="repset_limit.md" class="xref" title="DB_ENV-&gt;rep_set_limit()">DB_ENV-&gt;rep_set_limit()</a>
