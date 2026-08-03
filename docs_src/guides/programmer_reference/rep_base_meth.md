---
title: "Base API Methods"
api-name: "Base API Methods"
source: docs/programmer_reference/rep_base_meth.html
---
## Base API Methods

Base API applications use the following Berkeley DB methods.

<span class="term"> <a href="../../api/c/reptransport.md" class="olink">DB_ENV-&gt;rep_set_transport()</a> </span>  
The <a href="../../api/c/reptransport.md" class="olink">DB_ENV-&gt;rep_set_transport()</a> method configures the replication system's communications infrastructure.

<span class="term"> <a href="../../api/c/repstart.md" class="olink">DB_ENV-&gt;rep_start()</a> </span>  
The <a href="../../api/c/repstart.md" class="olink">DB_ENV-&gt;rep_start()</a> method configures (or reconfigures) an existing database environment to be a replication master or client.

<span class="term"> <a href="../../api/c/repmessage.md" class="olink">DB_ENV-&gt;rep_process_message()</a> </span>  
The <a href="../../api/c/repmessage.md" class="olink">DB_ENV-&gt;rep_process_message()</a> method is used to process incoming messages from other environments in the replication group. For clients, it is responsible for accepting log records and updating the local databases based on messages from the master. For both the master and the clients, it is responsible for handling administrative functions (for example, the protocol for dealing with lost messages), and permitting new clients to join an active replication group. This method should only be called after the replication system's communications infrastructure has been configured via <a href="../../api/c/reptransport.md" class="olink">DB_ENV-&gt;rep_set_transport()</a>.

<span class="term"> <a href="../../api/c/repelect.md" class="olink">DB_ENV-&gt;rep_elect()</a> </span>  
The <a href="../../api/c/repelect.md" class="olink">DB_ENV-&gt;rep_elect()</a> method causes the replication group to elect a new master; it is called whenever contact with the master is lost and the application wants the remaining sites to select a new master.

<span class="term"> <a href="../../api/c/envevent_notify.md" class="olink">DB_ENV-&gt;set_event_notify()</a> </span>  
The <a href="../../api/c/envevent_notify.md" class="olink">DB_ENV-&gt;set_event_notify()</a> method is needed for applications to discover important replication-related events, such as the result of an election and appointment of a new master.

<span class="term"> <a href="../../api/c/reppriority.md" class="olink">DB_ENV-&gt;rep_set_priority()</a> </span>  
The <a href="../../api/c/reppriority.md" class="olink">DB_ENV-&gt;rep_set_priority()</a> method configures the local site's priority for the purpose of elections.

<span class="term"> <a href="../../api/c/repset_timeout.md" class="olink">DB_ENV-&gt;rep_set_timeout()</a> </span>  
This method optionally configures various timeout values. Otherwise default timeout values as specified in <a href="../../api/c/repset_timeout.md" class="olink">DB_ENV-&gt;rep_set_timeout()</a> are used.

<span class="term"> <a href="../../api/c/repset_limit.md" class="olink">DB_ENV-&gt;rep_set_limit()</a> </span>  
The <a href="../../api/c/repset_limit.md" class="olink">DB_ENV-&gt;rep_set_limit()</a> method imposes an upper bound on the amount of data that will be sent in response to a single call to <a href="../../api/c/repmessage.md" class="olink">DB_ENV-&gt;rep_process_message()</a>. During client recovery, that is, when a replica site is trying to synchronize with the master, clients may ask the master for a large number of log records. If it is going to harm an application for the master message loop to remain busy for an extended period transmitting records to the replica, then the application will want to use <a href="../../api/c/repset_limit.md" class="olink">DB_ENV-&gt;rep_set_limit()</a> to limit the amount of data the master will send before relinquishing control and accepting other messages.

<span class="term"> <a href="../../api/c/repset_request.md" class="olink">DB_ENV-&gt;rep_set_request()</a> </span>  
This method sets a threshold for the minimum and maximum time that a client waits before requesting retransmission of a missing message.

In addition to the methods previously described, Base API applications may also call the following methods, as needed: <a href="../../api/c/repstat.md" class="olink">DB_ENV-&gt;rep_stat()</a>, <a href="../../api/c/repsync.md" class="olink">DB_ENV-&gt;rep_sync()</a> and <a href="../../api/c/repconfig.md" class="olink">DB_ENV-&gt;rep_set_config()</a>.
