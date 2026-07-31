---
title: "DB_SITE->get_address()"
api-name: "DB_SITE->get_address()"
source: docs/api_reference/C/dbsite_get_address.html
---
## DB_SITE-\>get_address()

``` c
#include <db.h>

int
DB_SITE->get_address(DB_SITE *site, const char **hostp, u_int *portp); 
```

The `DB_SITE->get_address()` method returns a replication site's network address. That is, this method returns the site's hostname and port.

The `DB_SITE->get_address()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### hostp

References memory into which is copied a pointer to the internal storage of the host name.

#### portp

References memory into which the port number will be copied.

### Class

<a href="db_site.md" class="link" title="The DB_SITE Handle">DB_SITE</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
