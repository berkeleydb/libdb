---
title: "DB_SITE->remove()"
api-name: "DB_SITE->remove()"
source: docs/api_reference/C/dbsite_remove.html
---
## DB_SITE-\>remove()

``` c
#include <db.h>

int
DB_SITE->remove(DB_SITE *site); 
```

The `DB_SITE->remove()` method removes the site from the replication group. If called at the master site, repmgr updates the membership database directly. If called from a client, this method causes a request to be sent to the master to perform the operation. The method then awaits confirmation.

The DB_SITE handle must not be accessed again after this method is called, regardless of the return value.

The `DB_SITE->remove()` method returns a non-zero error value on failure and 0 on success.

### Errors

The `DB_SITE->remove()` method may fail and return one of the following non-zero errors:

#### DB_REP_UNAVAIL

The master updated the database but did not receive enough acknowledgements from clients sufficient to meet the current ack policy or there was an attempt to remove the current master site from the replication group.

### Class

<a href="db_site.md" class="link" title="The DB_SITE Handle">DB_SITE</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
