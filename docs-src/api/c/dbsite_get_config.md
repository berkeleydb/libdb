---
title: "DB_SITE->get_config()"
api-name: "DB_SITE->get_config()"
source: docs/api_reference/C/dbsite_get_config.html
---
## DB_SITE-\>get_config()

``` c
#include <db.h>

int
DB_SITE->get_config(DB_SITE *site, u_int32_t which, 
                    u_int32_t *valuep); 
```

The `DB_SITE->get_config()` method returns whether the specified **which** parameter is currently set. See the <a href="dbsite_set_config.md" class="xref" title="DB_SITE-&gt;set_config()">DB_SITE-&gt;set_config()</a> method for the configuration flags that can be set for a DB_SITE handle.

The `DB_SITE->get_config()` method may be called at any time during the life of the application.

The `DB_SITE->get_config()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### which

The **which** parameter is the configuration flag to check. See the <a href="dbsite_set_config.md" class="xref" title="DB_SITE-&gt;set_config()">DB_SITE-&gt;set_config()</a> method for a list of configuration flags that you can provide to this parameter.

#### valuep

The **valuep** parameter references memory into which the configuration of the specified **which** parameter is copied.

If the returned value is zero, the parameter is off; otherwise it is on.

### Class

<a href="db_site.md" class="link" title="The DB_SITE Handle">DB_SITE</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>, <a href="dbsite_set_config.md" class="xref" title="DB_SITE-&gt;set_config()">DB_SITE-&gt;set_config()</a>
