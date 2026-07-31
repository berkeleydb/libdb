---
title: "DB_ENV->set_event_notify()"
api-name: "DB_ENV->set_event_notify()"
source: docs/api_reference/C/envevent_notify.html
---
## DB_ENV-\>set_event_notify()

``` c
#include <db.h>

int
DB_ENV->set_event_notify(DB_ENV *dbenv,
    void (*db_event_fcn)(DB_ENV *dbenv, u_int32_t event, 
    void *event_info));  
```

The `DB_ENV->set_event_notify()` method configures a callback function which is called to notify the process of specific Berkeley DB events.

### Note

Berkeley DB is not re-entrant. Callback functions should not attempt to make library calls (for example, to release locks or close open handles). Re-entering Berkeley DB is not guaranteed to work correctly, and the results are undefined.

The `DB_ENV->set_event_notify()` method configures operations performed using the specified <a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a> handle, not all operations performed on the underlying database environment.

The `DB_ENV->set_event_notify()` method may be called at any time during the life of the application.

The `DB_ENV->set_event_notify()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### db_event_fcn

The **db_event_fcn** parameter is the application's event notification function. The function takes three parameters:

- `dbenv`

  The **dbenv** parameter is the enclosing database environment handle.

- `event`

  The **event** parameter is one of the following values:

  - `DB_EVENT_PANIC`

    Errors can occur in the Berkeley DB library where the only solution is to shut down the application and run recovery (for example, if Berkeley DB is unable to allocate heap memory). In such cases, the Berkeley DB methods will return <a href="../../programmer_reference/program_errorret.html#program_errorret.DB_RUNRECOVERY" class="olink">DB_RUNRECOVERY</a>. It is often easier to simply exit the application when such errors occur rather than gracefully return up the stack.

    When **event** is set to `DB_EVENT_PANIC`, the database environment has failed. All threads of control in the database environment should exit the environment, and recovery should be run.

  - `DB_EVENT_REG_ALIVE`

    Recovery is needed in an environment where the <a href="envopen.md#envopen_DB_REGISTER" class="link">DB_REGISTER</a> flag was specified on the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method and there is a process attached to the environment. The callback function is triggered once for each process attached.

    The **event_info** parameter points to a pid_t value containing the process identifier (pid) of the process the Berkeley DB library detects is attached to the environment.

  - `DB_EVENT_REG_PANIC`

    Recovery is needed in an environment where the <a href="envopen.md#envopen_DB_REGISTER" class="link">DB_REGISTER</a> flag was specified on the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method. All threads of control in the database environment should exit the environment.

    This event is different than the `DB_EVENT_PANIC` event because it can only be triggered when `DB_REGISTER` was specified. It can be used to distinguish between the case when a process dies in the environment and recovery is initiated versus the case when an error happened (for example, if Berkeley DB is unable to allocate heap memory)

  - `DB_EVENT_REP_CLIENT`

    The local site is now a replication client.

    This event is generated when the replication role changes to client, either from master or from being unset. The role is unset when an environment is first created and after an environment is recovered. This event is not generated when restarting replication in an environment that was previously a client and was opened without recovery.

  - `DB_EVENT_REP_CONNECT_BROKEN`

    A previously established replication message connection between the local site and a remote site has been broken. This event supplies the EID of the remote site, and an integer error code that identifies the reason the connection was broken.

    A non-zero error code indicates an unexpected condition such as a hardware failure or a protocol error. An application might respond by emitting an informational message or passing this information to other parts of the application using the `app_private` field. A zero error code indicates that the connection was cleanly closed by the other end. Replication Manager retries broken connections periodically until they are restored.

  - `DB_EVENT_REP_CONNECT_ESTD`

    A replication message connection has been established between the local site and a remote site. This event supplied the EID of the remote site.

  - `DB_EVENT_REP_CONNECT_TRY_FAILED`

    An attempt to establish a connection between the local site and a remote site has failed. This event supplies the EID of the remote site, and an integer error code that identifies the reason the connection attempt failed.

  - `DB_EVENT_REP_DUPMASTER`

    Replication Manager has detected a duplicate master situation, and has changed the local site to the client role as a result. If the <a href="repconfig.md#config_DB_REPMGR_CONF_ELECTIONS" class="link">DB_REPMGR_CONF_ELECTIONS</a> configuration parameter has been turned off, the application should now choose and assign the correct master site. If `DB_REPMGR_CONF_ELECTIONS` is turned on, the application may ignore this event.

    The `DB_EVENT_REP_DUPMASTER` event is provided only to applications configured for the replication manager.

  - `DB_EVENT_REP_ELECTED`

    The local replication site has just won an election. An application using the Base replication API should arrange for a call to the <a href="repstart.md" class="xref" title="DB_ENV-&gt;rep_start()">DB_ENV-&gt;rep_start()</a> method after receiving this event, to reconfigure the local environment as a replication master.

    Replication Manager applications may safely ignore this event. The Replication Manager calls <a href="repstart.md" class="xref" title="DB_ENV-&gt;rep_start()">DB_ENV-&gt;rep_start()</a> automatically on behalf of the application when appropriate (resulting in firing of the DB_EVENT_REP_MASTER event).

  - `DB_EVENT_REP_ELECTION_FAILED`

    Replication Manager tried to run an election to choose a master site, but the election failed due to lack of timely participation by a sufficient number of other sites. Replication Manager will automatically retry the election later. This event is for information only.

    The `DB_EVENT_REP_ELECTION_FAILED` event is provided only to applications configured for the replication manager.

  - `DB_EVENT_REP_ELECTION_STARTED`

    Replication Manager has started an election to choose a master site.

  - `DB_EVENT_REP_INIT_DONE`

    Replication Manager has completed an internal initialization procedure.

  - `DB_EVENT_REP_JOIN_FAILURE`

    The local client site is unable to synchronize with a new master, possibly because the client has turned off automatic internal initialization by setting the <a href="repconfig.md#config_DB_REP_CONF_AUTOINIT" class="link">DB_REP_CONF_AUTOINIT</a> flag to `0`.

  - `DB_EVENT_REP_LOCAL_SITE_REMOVED`

    The local site has been removed from the replication group.

  - `DB_EVENT_REP_MASTER`

    The local site is now the master site of its replication group. It is the application's responsibility to begin acting as the master environment.

    This event is generated when the replication role changes to master, either from client or from being unset. The role is unset when an environment is first created and after an environment is recovered. This event is not generated when restarting replication in an environment that was previously a master and was opened without recovery.

  - `DB_EVENT_REP_MASTER_FAILURE`

    A Replication Manager client site has detected the loss of connection to the master site. If the <a href="repconfig.md#config_DB_REPMGR_CONF_ELECTIONS" class="link">DB_REPMGR_CONF_ELECTIONS</a> configuration parameter is turned on, Replication Manager will automatically start an election in order to choose a new master. In this case, this event may be ignored.

    When `DB_REPMGR_CONF_ELECTIONS` is turned off, the application should choose and assign a new master. Failure to do so means that your replication group has no master, and so it cannot service write requests.

    The `DB_EVENT_REP_MASTER_FAILURE` event is provided only to applications configured for the replication manager.

  - `DB_EVENT_REP_NEWMASTER`

    The replication group of which this site is a member has just established a new master; the local site is not the new master. The **event_info** parameter points to an integer containing the environment ID of the new master.

  - `DB_EVENT_REP_PERM_FAILED`

    The replication manager did not receive enough acknowledgements (based on the acknowledgement policy configured with <a href="repmgrset_ack_policy.md" class="xref" title="DB_ENV-&gt;repmgr_set_ack_policy()">DB_ENV-&gt;repmgr_set_ack_policy()</a> ) to ensure a transaction's durability within the replication group. The transaction will be flushed to the master's local disk storage for durability.

    The `DB_EVENT_REP_PERM_FAILED` event is provided only to applications configured for the replication manager.

  - `DB_EVENT_REP_SITE_ADDED`

    A new site has joined the group. The **event_info** parameter points to an integer containing the environment ID of the new site.

  - `DB_EVENT_REP_SITE_REMOVED`

    An existing remote site has been removed from the group. The **event_info** parameter points to an integer containing the environment ID of the site that was removed.

  - `DB_EVENT_REP_STARTUPDONE`

    The client has completed startup synchronization and is now processing live log records received from the master.

  - `DB_EVENT_WRITE_FAILED`

    A Berkeley DB write to stable storage failed.

- `event_info`

  The **event_info** parameter may reference memory which contains additional information describing an event. By default, **event_info** is NULL; specific events may pass non-NULL values, in which case the event will also describe the memory's structure.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="env.md#envlist" class="xref" title="Database Environments and Related Methods">Database Environments and Related Methods</a>
