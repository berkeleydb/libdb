---
title: "DB_SITE->get_eid()"
api-name: "DB_SITE->get_eid()"
source: docs/api_reference/C/dbsite_get_eid.html
---
## DB_SITE-\>get_eid()

``` c
#include <db.h>

int
DB_SITE->get_eid(DB_SITE *site, int *eidp); 
```

The `DB_SITE->get_eid()` method returns a replication site's Environment ID (EID). This method can be called only after environment open.

The `DB_SITE->get_eid()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### eidp

References memory into which the EID will be copied.

### Errors

The `DB_SITE->get_eid()` method may fail and return one of the following non-zero errors:

#### EINVAL

If an invalid flag value or parameter was specified.

### Class

<a href="db_site.md" class="link" title="The DB_SITE Handle">DB_SITE</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
