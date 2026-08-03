---
title: "DB_ENV->repmgr_stat()"
api-name: "DB_ENV->repmgr_stat()"
source: docs/api_reference/C/repmgrstat.html
---
## DB_ENV-\>repmgr_stat()

``` c
#include <db.h>

int
DB_ENV->repmgr_stat(DB_ENV *env, DB_REPMGR_STAT **statp, 
    u_int32_t flags);  
```

The `DB_ENV->repmgr_stat()` method returns the Replication Manager statistics.

The `DB_ENV->repmgr_stat()` method creates a statistical structure of type `DB_REPMGR_STAT` and copies a pointer to it into a user-specified memory location.

Statistical structures are stored in allocated memory. If application-specific allocation routines have been declared (see <a href="envset_alloc.md" class="xref" title="DB_ENV-&gt;set_alloc()">DB_ENV-&gt;set_alloc()</a> for more information), they are used to allocate the memory; otherwise, the standard C library **malloc**(3) is used. The caller is responsible for deallocating the memory. To deallocate the memory, free the memory reference; references inside the returned memory need not be individually freed.

The following `DB_REPMGR_STAT` fields will be filled in:

- **uintmax_t st_connect_fail;**

  The number of times an attempt to open a new TCP/IP connection failed.

- **uintmax_t st_connection_drop;**

  The number of times an existing TCP/IP connection failed.

- **uintmax_t st_msgs_dropped;**

  The number of outgoing messages that were completely dropped, because the outgoing message queue was full. (Berkeley DB replication is tolerant of dropped messages, and will automatically request retransmission of any missing messages as needed.)

- **uintmax_t st_msgs_queued;**

  The number of outgoing messages which could not be transmitted immediately, due to a full network buffer, and had to be queued for later delivery.

- **uintmax_t st_perm_failed;**

  The number of times a message critical for maintaining database integrity (for example, a transaction commit), originating at this site, did not receive sufficient acknowledgement from clients, according to the configured acknowledgement policy and acknowledgement timeout.

- **uintmax_t st_elect_threads;**

  The number of currently active election threads.

- **uintmax_t st_max_elect_threads;**

  The number of election threads for which space is reserved.

The `DB_ENV->repmgr_stat()` method may not be called before the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called.

The `DB_ENV->repmgr_stat()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### statp

The **statp** parameter references memory into which a pointer to the allocated statistics structure is copied.

#### flags

The **flags** parameter must be set to 0 or the following value:

- `DB_STAT_CLEAR`

  Reset statistics after returning their values.

### Errors

The `DB_ENV->repmgr_stat()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the method was called before <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> was called; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
