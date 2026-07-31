---
title: "Synchronizing with a master"
api-name: "Synchronizing with a master"
source: docs/programmer_reference/rep_mastersync.html
---
## Synchronizing with a master

<span class="sect2"> [Delaying client synchronization](rep_mastersync.md#rep_delay_sync) </span>

<span class="sect2"> [Client-to-client synchronization](rep_mastersync.md#rep_c2c_sync) </span>

<span class="sect2"> [Blocked client operations](rep_mastersync.md#idp52488504) </span>

<span class="sect2"> [Clients too far out-of-date to synchronize](rep_mastersync.md#idp52510624) </span>

When a client detects a new replication group master, the client must synchronize with the new master before the client can process new database changes. Synchronizing is a heavyweight operation which can place a burden on both the client and the master. There are several controls an application can use to reduce the synchronization burden.

### Delaying client synchronization

When a replication group has a new master, either as specified by the application or as a result of winning an election, all clients in the replication group must synchronize with the new master. This can strain the resources of the new master since a large number of clients may be attempting to communicate with and transfer records from the master. Client applications wanting to delay client synchronization should call the <a href="../../api/c/repconfig.md" class="olink">DB_ENV-&gt;rep_set_config()</a> method with the <a href="../../api/c/repconfig.md#config_DB_REP_CONF_DELAYCLIENT" class="olink">DB_REP_CONF_DELAYCLIENT</a> flag. The application will be notified of the establishment of the new master as usual, but the client will not proceed to synchronize with the new master.

Applications learn of a new master via the <a href="../../api/c/envevent_notify.md#event_notify_DB_EVENT_REP_NEWMASTER" class="olink">DB_EVENT_REP_NEWMASTER</a> event.

Client applications choosing to delay synchronization in this manner are responsible for synchronizing the client environment at some future time using the <a href="../../api/c/repsync.md" class="olink">DB_ENV-&gt;rep_sync()</a> method.

### Client-to-client synchronization

Instead of synchronizing with the new master, it is sometimes possible for a client to synchronize with another client. Berkeley DB initiates synchronization at the client by sending a request message via the transport call-back function of the communication infrastructure. The message is destined for the master site, but is also marked with a <a href="../../api/c/reptransport.md#transport_DB_REP_ANYWHERE" class="olink">DB_REP_ANYWHERE</a> flag. The application may choose to send such a request to another client, or to ignore the flag, sending it to its indicated destination.

Furthermore, when the other client receives such a request it may be unable to satisfy it. In this case it will reply to the requesting client, telling it that it is unable to provide the requested information. The requesting client will then re-issue the request. Additionally, if the original request never reaches the other client, the requesting client will again re-issue the request. In either of these cases the message will be marked with the <a href="../../api/c/reptransport.md#transport_DB_REP_REREQUEST" class="olink">DB_REP_REREQUEST</a> flag. The application may continue trying to find another client to service the request, or it may give up and simply send it to the master (that is, the environment ID explicitly specified to the transport function).

Replication Manager allows an application to designate one or more remote sites (called its "peers") to receive client-to-client requests. You do this by setting the `DB_REPMGR_PEER` parameter using the <a href="../../api/c/dbsite_set_config.md" class="olink">DB_SITE-&gt;set_config()</a> method. Replication Manager always tries to send requests marked with the <a href="../../api/c/reptransport.md#transport_DB_REP_ANYWHERE" class="olink">DB_REP_ANYWHERE</a> flag to a peer, if available. However, it always sends a <a href="../../api/c/reptransport.md#transport_DB_REP_REREQUEST" class="olink">DB_REP_REREQUEST</a> to the master site.

Base API applications have complete freedom in choosing where to send these <a href="../../api/c/reptransport.md#transport_DB_REP_ANYWHERE" class="olink">DB_REP_ANYWHERE</a> requests, and in deciding how to handle <a href="../../api/c/reptransport.md#transport_DB_REP_REREQUEST" class="olink">DB_REP_REREQUEST</a>.

The delayed synchronization and client-to-client synchronization features allow applications to do load balancing within replication groups. For example, consider a replication group with 5 sites, A, B, C, D and E. Site E just crashed, and site A was elected master. Sites C and D have been configured for delayed synchronization. When site B is notified that site A is a new master, it immediately synchronizes. When B finishes synchronizing with the master, the application calls the <a href="../../api/c/repsync.md" class="olink">DB_ENV-&gt;rep_sync()</a> method on sites C and D to cause them to synchronize as well. Sites C and D (and E, when it has finished rebooting) can send their requests to site B, and B then bears the brunt of the work and network traffic for synchronization, making master site A available to handle the normal application load and any write requests paused by the election.

### Blocked client operations

Clients in the process of synchronizing with the master block access to Berkeley DB operations during some parts of that process. By default, most Berkeley DB methods will block until client synchronization is complete, and then the method call proceeds.

Client applications which cannot wait and would prefer an immediate error return instead of blocking, should call the <a href="../../api/c/repconfig.md" class="olink">DB_ENV-&gt;rep_set_config()</a> method with the <a href="../../api/c/repconfig.md#config_DB_REP_CONF_NOWAIT" class="olink">DB_REP_CONF_NOWAIT</a> flag. This configuration causes <a href="../../api/c/db.md" class="olink">DB</a> method calls to immediately return a <a href="../../api/c/dbput.md#dbput_DB_REP_LOCKOUT" class="olink">DB_REP_LOCKOUT</a> error instead of blocking, if the client is currently synchronizing with the master.

### Clients too far out-of-date to synchronize

Clients attempting to synchronize with the master may discover that synchronization is not possible because the client no longer has any overlapping information with the master site. By default, the master and client automatically detect this state and perform an internal initialization of the client. Because internal initialization requires transfer of entire databases to the client, it can take a relatively long period of time and may require database handles to be reopened in the client applications.

Client applications which cannot wait or would prefer to do a hot backup instead of performing internal initialization, should call the <a href="../../api/c/repconfig.md" class="olink">DB_ENV-&gt;rep_set_config()</a> method to turn off the <a href="../../api/c/repconfig.md#config_DB_REP_CONF_AUTOINIT" class="olink">DB_REP_CONF_AUTOINIT</a> flag. Turning off this configuration flag causes Berkeley DB to return <a href="../../api/c/repmessage.md#repmsg_DB_REP_JOIN_FAILURE" class="olink">DB_REP_JOIN_FAILURE</a> to the application instead of performing internal initialization.
