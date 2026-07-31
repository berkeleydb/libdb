---
title: "DB_ENV->rep_sync()"
api-name: "DB_ENV->rep_sync()"
source: docs/api_reference/C/repsync.html
---
## DB_ENV-\>rep_sync()

``` c
#include <db.h>

int
DB_ENV->rep_sync(DB_ENV *env, u_int32_t flags);  
```

The `DB_ENV->rep_sync()` method forces master synchronization to begin for this client. This method is the other half of setting the <a href="repconfig.md#config_DB_REP_CONF_DELAYCLIENT" class="link">DB_REP_CONF_DELAYCLIENT</a> flag via the <a href="repconfig.md" class="xref" title="DB_ENV-&gt;rep_set_config()">DB_ENV-&gt;rep_set_config()</a> method.

If an application has configured delayed master synchronization, the application must synchronize explicitly (otherwise the client will remain out-of-date and will ignore all database changes forwarded from the replication group master). The `DB_ENV->rep_sync()` method may be called any time after the client application learns that the new master has been established (by receiving a <a href="envevent_notify.md#event_notify_DB_EVENT_REP_NEWMASTER" class="link">DB_EVENT_REP_NEWMASTER</a> event notification).

Before calling this method, the enclosing database environment must already have been opened by calling the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method and must already have been configured to send replication messages by calling the <a href="reptransport.md" class="xref" title="DB_ENV-&gt;rep_set_transport()">DB_ENV-&gt;rep_set_transport()</a> method.

The `DB_ENV->rep_sync()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flags

The **flags** parameter is currently unused, and must be set to 0.

### Errors

The `DB_ENV->rep_sync()` method may fail and return one of the following non-zero errors:

#### EINVAL

If the database environment was not already configured to communicate with a replication group by a call to <a href="reptransport.md" class="xref" title="DB_ENV-&gt;rep_set_transport()">DB_ENV-&gt;rep_set_transport()</a>; the database environment was not already opened; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
