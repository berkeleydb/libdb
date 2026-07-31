---
title: "DB_ENV->rep_start()"
api-name: "DB_ENV->rep_start()"
source: docs/api_reference/C/repstart.html
---
## DB_ENV-\>rep_start()

``` c
#include <db.h>

int
DB_ENV->rep_start(DB_ENV *env, DBT *cdata, u_int32_t flags);  
```

The `DB_ENV->rep_start()` method configures the database environment as a client or master in a group of replicated database environments.

The `DB_ENV->rep_start()` method is not called by most replication applications. It should only be called by Base API applications implementing their own network transport layer, explicitly holding replication group elections and handling replication messages outside of the Replication Manager framework.

Replication master environments are the only database environments where replicated databases may be modified. Replication client environments are read-only as long as they are clients. Replication client environments may be upgraded to be replication master environments in the case that the current master fails or there is no master present. If master leases are in use, this method cannot be used to appoint a master, and should only be used to configure a database environment as a master as the result of an election.

The enclosing database environment must already have been opened by calling the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method and must already have been configured to send replication messages by calling the <a href="reptransport.md" class="xref" title="DB_ENV-&gt;rep_set_transport()">DB_ENV-&gt;rep_set_transport()</a> method.

The `DB_ENV->rep_start()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### cdata

The **cdata** parameter is an opaque data item that is sent over the communication infrastructure when the client comes online (see <a href="../../programmer_reference/rep_newsite.html" class="olink">Connecting to a new site</a> for more information). If no such information is useful, **cdata** should be NULL.

#### flags

The **flags** parameter must be set to one of the following values:

- `DB_REP_CLIENT`

  Configure the environment as a replication client.

- `DB_REP_MASTER`

  Configure the environment as a replication master.

### Errors

The `DB_ENV->rep_start()` method may fail and return one of the following non-zero errors:

#### DB_REP_UNAVAIL

If the flags parameter was passed as DB_REP_MASTER but the database environment cannot currently become the replication master because it is temporarily initializing and is incomplete.

#### EINVAL

If the database environment was not already configured to communicate with a replication group by a call to <a href="reptransport.md" class="xref" title="DB_ENV-&gt;rep_set_transport()">DB_ENV-&gt;rep_set_transport()</a>; the database environment was not already opened; this method is called from a Replication Manager application; outstanding master leases are granted; this method is used to appoint a new master when master leases are in use; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
