---
title: "DB_ENV->rep_set_config()"
api-name: "DB_ENV->rep_set_config()"
source: docs/api_reference/C/repconfig.html
---
## DB_ENV-\>rep_set_config()

``` c
#include <db.h>

int
DB_ENV->rep_set_config(DB_ENV *env, u_int32_t which, int onoff);  
```

The `DB_ENV->rep_set_config()` method configures the Berkeley DB replication subsystem.

The database environment's replication subsystem may also be configured using the environment's <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file. The syntax of the entry in that file is a single line with the string "rep_set_config", one or more whitespace characters, and the method **which** parameter as a string and optionally one or more whitespace characters, and the string "on" or "off". If the optional string is omitted, the default is "on"; for example, "rep_set_config DB_REP_CONF_NOWAIT" or "rep_set_config DB_REP_CONF_NOWAIT on". Because the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is read when the database environment is opened, it will silently overrule configuration done before that time.

The `DB_ENV->rep_set_config()` method configures a database environment, not only operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle.

The `DB_ENV->rep_set_config()` method may not be called to set in-memory replication after the environment is opened using the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method. This method may also not be called to set master leases after the <a href="repstart.md" class="xref" title="DB_ENV-&gt;rep_start()">DB_ENV-&gt;rep_start()</a> or <a href="repmgrstart.md" class="xref" title="DB_ENV-&gt;repmgr_start()">DB_ENV-&gt;repmgr_start()</a> methods are called. For all other **which** parameters, this method may be called at any time during the life of the application.

The `DB_ENV->rep_set_config()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### which

The **which** parameter must be set to one of the following values:

- `DB_REP_CONF_AUTOINIT`

  The replication master will automatically re-initialize outdated clients. This option is turned on by default.

- `DB_REP_CONF_BULK`

  The replication master sends groups of records to the clients in a single network transfer.

- `DB_REP_CONF_DELAYCLIENT`

  The client should delay synchronizing to a newly declared master. Clients configured in this way will remain unsynchronized until the application calls the <a href="repsync.md" class="xref" title="DB_ENV-&gt;rep_sync()">DB_ENV-&gt;rep_sync()</a> method.

- `DB_REP_CONF_INMEM`

  Store internal replication information in memory only.

  By default, replication creates files in the environment home directory to preserve some internal information. If this configuration flag is turned on, replication only stores this internal information in-memory and cannot keep persistent state across a site crash or reboot. This results in the following limitations:

  - A master site should not reappoint itself master immediately after crashing or rebooting because the application would incur a slightly higher risk of client crashes. The former master site should rejoin the replication group as a client. The application should either hold an election or appoint a different site to be the next master.

  - An application has a slightly higher risk that elections will fail or be unable to complete. Calling additional elections should eventually yield a winner.

  - An application has a slight risk that the wrong site may win an election, resulting in the loss of some data. This is consistent with the general loss of data durability when running in-memory.

  - Replication Manager applications will no longer maintain group membership information persistently on-disk. For more information, see <a href="../../programmer_reference/rep_filename.html" class="olink">Managing Replication Files</a> in the *Berkeley DB Programmer's Reference Guide*.

  This configuration flag can only be turned on before the environment is opened with the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method. Its value cannot be changed while the environment is open. All sites in the replication group should have the same value for this configuration flag.

- `DB_REP_CONF_LEASE`

  Master leases will be used for this site.

  Configuring this option may result in `DB_REP_LEASE_EXPIRED` error returns from the <a href="dbget.md" class="xref" title="DB-&gt;get()">DB-&gt;get()</a> and <a href="dbcget.md" class="xref" title="DBcursor-&gt;get()">DBcursor-&gt;get()</a> methods when attempting to read entries from a database after the site's master lease has expired.

  This configuration flag may not be set after the <a href="repmgrstart.md" class="xref" title="DB_ENV-&gt;repmgr_start()">DB_ENV-&gt;repmgr_start()</a> method or the <a href="repstart.md" class="xref" title="DB_ENV-&gt;rep_start()">DB_ENV-&gt;rep_start()</a> method is called. All sites in the replication group should have the same value for this configuration flag.

- `DB_REP_CONF_NOWAIT`

  Berkeley DB method calls that would normally block while clients are in recovery will return errors immediately.

- `DB_REPMGR_CONF_ELECTIONS`

  Replication Manager automatically runs elections to choose a new master when the old master appears to have become disconnected. This option is turned on by default.

  If this option is turned off, the application is responsible for assigning the new master explicitly, by calling the `DB_ENV->repmgr_start()` method.

  ### Caution

  Most Replication Manager applications should accept the default automatic behavior. Allowing two sites in a replication group to act as master simultaneously can lead to loss of data.

  In an application with multiple processes per database environment, only the main replication process may change this configuration setting.

- `DB_REPMGR_CONF_2SITE_STRICT`

  Replication Manager observes the strict "majority" rule in managing elections, even in a group with only 2 sites. This means the client in a 2-site group will be unable to take over as master if the original master fails or becomes disconnected. (See the <a href="../../programmer_reference/rep_twosite.html" class="olink">Special considerations for two-site replication groups</a> section in the *Berkeley DB Programmer's Reference Guide* for more information.) Both sites in the replication group should have the same value for this configuration flag. This option is turned on by default.

#### onoff

If the **onoff** parameter is zero, the configuration flag is turned off. Otherwise, it is turned on. Most configuration flags are turned off by default, exceptions are noted above.

### Errors

The `DB_ENV->rep_set_config()` method may fail and return one of the following non-zero errors:

#### EINVAL

If setting in-memory replication after the database environment is already opened; if setting master leases after replication is started; if setting the 2-site strict majority rule for a Base API application; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
