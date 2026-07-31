---
title: "DB_ENV->repmgr_site_by_eid()"
api-name: "DB_ENV->repmgr_site_by_eid()"
source: docs/api_reference/C/repmgr_site_by_eid.html
---
## DB_ENV-\>repmgr_site_by_eid()

``` c
#include <db.h>

int
DB_ENV->repmgr_site_by_eid(DB_ENV *env, int eid,
    DB_SITE **sitep);  
```

The `DB_ENV->repmgr_site_by_eid()` method returns a DB_SITE handle based on the site's Environment ID value. You use the DB_SITE handle to configure and manage replication sites.

This method allocates memory for the handle, returning a pointer to the structure in the memory to which **sitep** refers. To release the allocated memory and discard the handle, call the <a href="dbsite_close.md" class="xref" title="DB_SITE-&gt;close()">DB_SITE-&gt;close()</a> method.

The `DB_ENV->repmgr_site_by_eid()` method may be called at any time after environment open time.

The `DB_ENV->repmgr_site_by_eid()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### eid

The Environment ID of the site for which you want to create the DB_SITE handle. You can obtain a site's EID by using the <a href="dbsite_get_eid.md" class="xref" title="DB_SITE-&gt;get_eid()">DB_SITE-&gt;get_eid()</a> method.

#### sitep

References memory into which a pointer to the allocated handle is copied.

### Errors

The `DB_ENV->repmgr_site()` method may fail and return one of the following non-zero errors:

#### EINVAL

If this method is called from a Base API application, or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
