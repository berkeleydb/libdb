---
title: "DB_ENV->rep_get_timeout()"
api-name: "DB_ENV->rep_get_timeout()"
source: docs/api_reference/C/repget_timeout.html
---
## DB_ENV-\>rep_get_timeout()

``` c
#include <db.h>

int
DB_ENV->rep_get_timeout(DB_ENV *env, int which, u_int32_t *timeoutp);  
```

The `DB_ENV->rep_get_timeout()` method returns the timeout value for the specified **which** parameter. Timeout values can be managed using the <a href="repset_timeout.md" class="xref" title="DB_ENV-&gt;rep_set_timeout()">DB_ENV-&gt;rep_set_timeout()</a> method.

The `DB_ENV->rep_get_timeout()` method may be called at any time during the life of the application.

The `DB_ENV->rep_get_timeout()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### which

The **which** parameter is the timeout for which the value is being returned. See the <a href="repset_timeout.md" class="xref" title="DB_ENV-&gt;rep_set_timeout()">DB_ENV-&gt;rep_set_timeout()</a> method for a list of timeouts that you can provide to this parameter.

#### timeoutp

The **timeoutp** parameter references memory into which the timeout value of the specified **which** parameter is copied.

The returned timeout value is in microseconds.

### Errors

The `DB_ENV->rep_get_timeout()` method may fail and return one of the following non-zero errors:

#### EINVAL

An invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>, <a href="repset_timeout.md" class="xref" title="DB_ENV-&gt;rep_set_timeout()">DB_ENV-&gt;rep_set_timeout()</a>
