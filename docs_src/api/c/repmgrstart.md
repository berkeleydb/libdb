---
title: "DB_ENV->repmgr_start()"
api-name: "DB_ENV->repmgr_start()"
source: docs/api_reference/C/repmgrstart.html
---
## DB_ENV-\>repmgr_start()

``` c
#include <db.h>

int
DB_ENV->repmgr_start(DB_ENV *env, int nthreads, u_int32_t flags);  
```

The `DB_ENV->repmgr_start()` method starts the Replication Manager.

There are two ways to build Berkeley DB replication applications: the most common approach is to use the Berkeley DB library Replication Manager, where the Berkeley DB library manages the replication group, including network transport, all replication message processing and acknowledgment, and group elections. Applications using the Replication Manager generally make the following calls:

1.  Use <a href="repmgr_site.md" class="xref" title="DB_ENV-&gt;repmgr_site()">DB_ENV-&gt;repmgr_site()</a> to obtain a DB_SITE handle, then use that handle to configure the sites in your replication group.

    1.  Use <a href="dbsite_set_config.md" class="xref" title="DB_SITE-&gt;set_config()">DB_SITE-&gt;set_config()</a> to configure sites in your replication group.

    2.  Use <a href="dbsite_remove.md" class="xref" title="DB_SITE-&gt;remove()">DB_SITE-&gt;remove()</a> to remove a site from the replication group.

2.  Call <a href="repmgrset_ack_policy.md" class="xref" title="DB_ENV-&gt;repmgr_set_ack_policy()">DB_ENV-&gt;repmgr_set_ack_policy()</a> to configure the message acknowledgment policy which best supports the replication group's transactional needs.

3.  Call <a href="reppriority.md" class="xref" title="DB_ENV-&gt;rep_set_priority()">DB_ENV-&gt;rep_set_priority()</a> to configure the local site's election priority.

4.  Call `DB_ENV->repmgr_start()` to start the replication application.

For more information on building Replication Manager applications, please see the *Replication Getting Started Guide* included in the Berkeley DB documentation.

Applications with special needs (for example, applications using network protocols not supported by the Berkeley DB Replication Manager), must perform additional configuration and call other Berkeley DB replication Base API methods. For more information on building Base API applications, please see the <a href="../../guides/programmer_reference/rep_base_meth.md" class="olink">Base API Methods</a> section in the *Berkeley DB Programmer's Reference Guide*.

Starting the Replication Manager consists of opening the TCP/IP listening socket to accept incoming connections, and starting all necessary background threads. When multiple processes share a database environment, only one process can open the listening socket; the `DB_ENV->repmgr_start()` method automatically opens the socket in the first process to call it, and skips this step in the later calls from other processes.

The `DB_ENV->repmgr_start()` method may not be called before the <a href="envopen.md" class="xref" title="DB_ENV-&gt;open()">DB_ENV-&gt;open()</a> method is called to open. In addition, this method may not be called before your replication sites have been configured using the <a href="db_site.md" class="link" title="The DB_SITE Handle">DB_SITE</a> class. In addition, the local environment must be opened with the <a href="envopen.md#envopen_DB_THREAD" class="link">DB_THREAD</a> flag set.

The `DB_ENV->repmgr_start()` method will return `DB_REP_IGNORE` as an informational, non-error return code, if another process has previously become the TCP/IP listener (though the current call has nevertheless successfully started Replication Manager's background threads). Unless otherwise specified, the `DB_ENV->repmgr_start()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### nthreads

Specify the number of threads of control created and dedicated to processing replication messages. In addition to these message processing threads, the Replication Manager creates and manages a few of its own threads of control. The TCP/IP listener process can change this value after the Replication Manager is started with a subsequent call to the `DB_ENV->repmgr_start()` method.

#### flags

The **flags** parameter must be set to one of the following values when first starting the Replication Manager:

- `DB_REP_MASTER`

  Start as a master site, and do not call for an election. Note there must never be more than a single master in any replication group, and only one site at a time should ever be started with the DB_REP_MASTER flag specified.

- `DB_REP_CLIENT`

  Start as a client site, and do not call for an election.

- `DB_REP_ELECTION`

  Start as a client, and call for an election if no master is found.

If the Replication Manager is already started, a **flags** value of 0 can be used when making a subsequent call to change the value of **nthreads**.

### Errors

The `DB_ENV->repmgr_start()` method may fail and return one of the following non-zero errors:

#### DB_REP_UNAVAIL

The local site tried to join the group, but was unable to do so for some reason (because a master site is not available, or because insufficient replicas are running to acknowledge the new site). When that happens the application should pause and retry adding the site until it completes successfully.

#### EINVAL

If the database environment was not already opened or was opened without the `DB_THREAD` flag set; a local site has not already been configured, this method is called from a Base API application; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
