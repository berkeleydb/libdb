---
title: "DB_ENV->rep_set_timeout()"
api-name: "DB_ENV->rep_set_timeout()"
source: docs/api_reference/C/repset_timeout.html
---
## DB_ENV-\>rep_set_timeout()

``` c
#include <db.h>

int
DB_ENV->rep_set_timeout(DB_ENV *env, int which, u_int32_t timeout);  
```

The `DB_ENV->rep_set_timeout()` method specifies a variety of replication timeout values.

The database environment's replication subsystem may also be configured using the environment's <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "rep_set_timeout", one or more whitespace characters, and the **which** parameter specified as a string and the timeout specified as two parts. For example, "rep_set_timeout DB_REP_CONNECTION_RETRY 15000000" specifies the connection retry timeout for 15 seconds. Because the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

The `DB_ENV->rep_set_timeout()` method configures a database environment, not only operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle.

The `DB_ENV->rep_set_timeout()` method may not be called to set the master lease timeout after the <a href="repmgrstart.md" class="xref" title="DB_ENV-&gt;repmgr_start()">DB_ENV-&gt;repmgr_start()</a> method or the <a href="repstart.md" class="xref" title="DB_ENV-&gt;rep_start()">DB_ENV-&gt;rep_start()</a> method is called. For all other timeouts, the `DB_ENV->rep_set_timeout()` method may be called at any time during the life of the application.

The `DB_ENV->rep_set_timeout()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### timeout

The **timeout** parameter is the timeout value. It must be specified as an unsigned 32-bit number of microseconds, limiting the maximum timeout to roughly 71 minutes.

#### which

The **which** parameter must be set to one of the following values:

- `DB_REP_ACK_TIMEOUT`

  Configure the amount of time the Replication Manager's transport function waits to collect enough acknowledgments from replication group clients, before giving up and returning a failure indication. The default wait time is 1 second.

- `DB_REP_CHECKPOINT_DELAY`

  Configure the amount of time a master site will delay between completing a checkpoint and writing a checkpoint record into the log. This delay allows clients to complete their own checkpoints before the master requires completion of them. The default is 30 seconds. If all databases in the environment, and the environment's transaction log, are configured to reside in memory (never preserved to disk), then, although checkpoints are still necessary, the delay is not useful and should be set to 0.

- `DB_REP_CONNECTION_RETRY`

  Configure the amount of time the Replication Manager will wait before trying to re-establish a connection to another site after a communication failure. The default wait time is 30 seconds.

- `DB_REP_ELECTION_TIMEOUT`

  The timeout period for an election. The default timeout is 2 seconds.

- `DB_REP_ELECTION_RETRY`

  Configure the amount of time the Replication Manager will wait before retrying a failed election. The default wait time is 10 seconds.

- `DB_REP_FULL_ELECTION_TIMEOUT`

  An optional configuration timeout period to wait for full election participation the first time the replication group finds a master. By default this option is turned off and normal election timeouts are used. (See the <a href="../../guides/programmer_reference/rep_elect.md" class="olink">Elections</a> section in the *Berkeley DB Programmer's Reference Guide* for more information.)

- `DB_REP_HEARTBEAT_MONITOR`

  The amount of time the Replication Manager, running at a client site, waits for some message activity on the connection from the master (heartbeats or other messages) before concluding that the connection has been lost. This timeout should be of longer duration than the `DB_REP_HEARTBEAT_SEND` timeout to ensure that heartbeats are not missed. When 0 (the default), no monitoring is performed.

- `DB_REP_HEARTBEAT_SEND`

  The frequency at which the Replication Manager, running at a master site, broadcasts a heartbeat message in an otherwise idle system. Heartbeat messages are used at client sites to monitor the connection to the master and to help request missing master changes in the absence of master activity. When 0 (the default), no heartbeat messages will be sent.

- `DB_REP_LEASE_TIMEOUT`

  Configure the amount of time a client grants its master lease to a master. When using master leases all sites in a replication group must use the same lease timeout value. There is no default value. If leases are desired, this method must be called prior to calling <a href="repstart.md" class="xref" title="DB_ENV-&gt;rep_start()">DB_ENV-&gt;rep_start()</a> method. See also <a href="repclockskew.md" class="xref" title="DB_ENV-&gt;rep_set_clockskew()">DB_ENV-&gt;rep_set_clockskew()</a> method, <a href="repconfig.md" class="xref" title="DB_ENV-&gt;rep_set_config()">DB_ENV-&gt;rep_set_config()</a> method or <a href="../../guides/programmer_reference/rep_lease.md" class="olink">Master leases</a>.

### Errors

The `DB_ENV->rep_set_timeout()` method may fail and return one of the following non-zero errors:

#### EINVAL

If setting the lease timeout and replication has already been started; if setting a Replication Manager timeout for a Base API application; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
