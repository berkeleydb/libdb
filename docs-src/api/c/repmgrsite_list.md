---
title: "DB_ENV->repmgr_site_list()"
api-name: "DB_ENV->repmgr_site_list()"
source: docs/api_reference/C/repmgrsite_list.html
---
## DB_ENV-\>repmgr_site_list()

``` c
#include <db.h>

int
DB_ENV->repmgr_site_list(DB_ENV *env,
    u_int *countp, DB_REPMGR_SITE **listp);
 
```

The `DB_ENV->repmgr_site_list()` method returns the status of the sites currently known by the Replication Manager.

The `DB_ENV->repmgr_site_list()` method creates a statistical structure of type `DB_REPMGR_SITE` and copies a pointer to it into a user-specified memory location.

Statistical structures are stored in allocated memory. If application-specific allocation routines have been declared (see <a href="envset_alloc.md" class="xref" title="DB_ENV-&gt;set_alloc()">DB_ENV-&gt;set_alloc()</a> for more information), they are used to allocate the memory; otherwise, the standard C library **malloc**(3) is used. The caller is responsible for deallocating the memory. To deallocate the memory, free the memory reference; references inside the returned memory need not be individually freed.

The following `DB_REPMGR_SITE` fields will be filled in:

- **int eid;**

  Environment ID assigned by the Replication Manager. This is the same value that is passed to the application's event notification function for the <a href="envevent_notify.md#event_notify_DB_EVENT_REP_NEWMASTER" class="link">DB_EVENT_REP_NEWMASTER</a> event.

- **char host\[\];**

  Null-terminated host name.

- **u_int port;**

  TCP/IP port number.

- **u_int32_t status;**

  Zero (if unknown), or one of the following constants: `DB_REPMGR_CONNECTED`, `DB_REPMGR_DISCONNECTED`.

- **u_int32_t flags;**

  Zero or a bitwise inclusive OR of the `DB_REPMGR_ISPEER` constant. The `DB_REPMGR_ISPEER` value means that the site is a possible client-to-client peer.

The `DB_ENV->repmgr_site_list()` method may be called only after the <a href="repmgrstart.md" class="xref" title="DB_ENV-&gt;repmgr_start()">DB_ENV-&gt;repmgr_start()</a> method has been called with a `0` return code.

The `DB_ENV->repmgr_site_list()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### countp

A count of the returned structures will be stored into the memory referenced by **countp**.

#### listp

A reference to an array of structures will be stored into the memory referenced by **listp**.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
