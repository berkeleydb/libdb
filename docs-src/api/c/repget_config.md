---
title: "DB_ENV->rep_get_config()"
api-name: "DB_ENV->rep_get_config()"
source: docs/api_reference/C/repget_config.html
---
## DB_ENV-\>rep_get_config()

``` c
#include <db.h>

int
DB_ENV->rep_get_config(DB_ENV *env, u_int32_t which, int *onoffp);  
```

The `DB_ENV->rep_get_config()` method returns whether the specified **which** parameter is currently set or not. See the <a href="repconfig.md" class="xref" title="DB_ENV-&gt;rep_set_config()">DB_ENV-&gt;rep_set_config()</a> method for the configuration flags that can be set for replication.

The `DB_ENV->rep_get_config()` method may be called at any time during the life of the application.

The `DB_ENV->rep_get_config()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### which

The **which** parameter is the configuration flag which is being checked. See the <a href="repconfig.md" class="xref" title="DB_ENV-&gt;rep_set_config()">DB_ENV-&gt;rep_set_config()</a> method for a list of configuration flags that you can provide to this parameter.

#### onoffp

The **onoffp** parameter references memory into which the configuration of the specified **which** parameter is copied.

If the returned **onoff** value is zero, the parameter is off; otherwise it is on.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>, <a href="repconfig.md" class="xref" title="DB_ENV-&gt;rep_set_config()">DB_ENV-&gt;rep_set_config()</a>
