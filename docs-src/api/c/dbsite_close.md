---
title: "DB_SITE->close()"
api-name: "DB_SITE->close()"
source: docs/api_reference/C/dbsite_close.html
---
## DB_SITE-\>close()

``` c
#include <db.h>

int
DB_SITE->close(DB_SITE *site); 
```

The `DB_SITE->close()` method deallocates the DB_SITE handle. The handle must not be accessed again after this method is called, regardless of the return value.

Use of this method does not in any way affect the configuration of the site to which the handle refers, or of the replication group in general.

All DB_SITE handles must be closed before the owning DB_ENV handle is closed.

The `DB_SITE->close()` method returns a non-zero error value on failure and 0 on success.

### Errors

The `DB_SITE->close()` method may fail and return one of the following non-zero errors:

### Class

<a href="db_site.md" class="link" title="The DB_SITE Handle">DB_SITE</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
