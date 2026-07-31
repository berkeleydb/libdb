---
title: "DB_ENV->repmgr_site()"
api-name: "DB_ENV->repmgr_site()"
source: docs/api_reference/C/repmgr_site.html
---
## DB_ENV-\>repmgr_site()

``` c
#include <db.h>

int
DB_ENV->repmgr_site(DB_ENV *env, const char *host,
    u_int port, DB_SITE **sitep, u_int32_t flags);  
```

The `DB_ENV->repmgr_site()` method returns a DB_SITE handle that defines a site's host/port network address. You use the DB_SITE handle to configure and manage replication sites.

This method allocates memory for the handle, returning a pointer to the structure in the memory to which **sitep** refers. To release the allocated memory and discard the handle, call the <a href="dbsite_close.md" class="xref" title="DB_SITE-&gt;close()">DB_SITE-&gt;close()</a> method.

You must use the exact same host identification string and port number to refer to a given site throughout your application and on each of its sites.

The `DB_ENV->repmgr_site()` method may be called at any time after the environment handle has been created.

The `DB_ENV->repmgr_site()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### host

The site's host identification string, generally a TCP/IP host name.

#### port

The port number on which the site is listening.

#### sitep

References memory into which a pointer to the allocated handle is copied.

#### flags

This parameter is currently unused, and must be set to 0.

### Errors

The `DB_ENV->repmgr_site()` method may fail and return one of the following non-zero errors:

#### EINVAL

If this method is called from a Base API application, or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
