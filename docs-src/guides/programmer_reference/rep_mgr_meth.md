---
title: "Replication Manager methods"
api-name: "Replication Manager methods"
source: docs/programmer_reference/rep_mgr_meth.html
---
## Replication Manager methods

Applications which use the Replication Manager support generally call the following Berkeley DB methods. The general pattern is to call various methods to configure Replication Manager, and then start it by calling <a href="../../api/c/repmgrstart.md" class="olink">DB_ENV-&gt;repmgr_start()</a>. Once this initialization is complete, the application rarely needs to call any of these methods. (A prime example of an exception to this rule would be the <a href="../../api/c/repsync.md" class="olink">DB_ENV-&gt;rep_sync()</a> method, if the application is <a href="rep_mastersync.md#rep_delay_sync" class="xref" title="Delaying client synchronization">Delaying client synchronization</a>.)

<span class="term"> <a href="../../api/c/db_site.md" class="olink">DB_SITE</a> </span>  
The <a href="../../api/c/db_site.md" class="olink">DB_SITE</a> handle is used to configure a site that belongs to the replication group. You can obtain a <a href="../../api/c/db_site.md" class="olink">DB_SITE</a> handle by calling the <a href="../../api/c/repmgr_site.md" class="olink">DB_ENV-&gt;repmgr_site()</a> method. When you do this, you provide the TCP/IP host name and port that the replication site uses for incoming connections.

Once you have the <a href="../../api/c/db_site.md" class="olink">DB_SITE</a> handle, you use the <a href="../../api/c/dbsite_set_config.md" class="olink">DB_SITE-&gt;set_config()</a> method to configure the handle. One of the things you can configure about the handle is whether it is the local site (using the `DB_LOCAL_SITE` parameter). You must configure one and only one <a href="../../api/c/db_site.md" class="olink">DB_SITE</a> handle to be a local site before you start replication.

You can also optionally configure <a href="../../api/c/db_site.md" class="olink">DB_SITE</a> handles for remote sites to help Replication Manager startup more efficiently. Note that it is usually not necessary for each site in the replication group initially to know about all other sites in the group. Sites can discover each other dynamically, as described in <a href="rep_newsite.md" class="xref" title="Connecting to a new site">Connecting to a new site</a>.

Once you have configured your <a href="../../api/c/db_site.md" class="olink">DB_SITE</a> handles, you start replication using <a href="../../api/c/repmgrstart.md" class="olink">DB_ENV-&gt;repmgr_start()</a>.

When you are shutting down your application, you must use the <a href="../../api/c/dbsite_close.md" class="olink">DB_SITE-&gt;close()</a> method to close all your open <a href="../../api/c/db_site.md" class="olink">DB_SITE</a> handles before you close your environment handles.

<span class="term"> <a href="../../api/c/repmgrset_ack_policy.md" class="olink">DB_ENV-&gt;repmgr_set_ack_policy()</a> </span>  
The <a href="../../api/c/repmgrset_ack_policy.md" class="olink">DB_ENV-&gt;repmgr_set_ack_policy()</a> method configures the acknowledgement policy to be used in the replication group, in other words, the behavior of the master with respect to acknowledgements for "permanent" messages, which implements the application's requirements for <a href="rep_trans.md" class="xref" title="Transactional guarantees">Transactional guarantees</a>. The current implementation requires all sites in the replication group to configure the same acknowledgement policy.

<span class="term"> <a href="../../api/c/reppriority.md" class="olink">DB_ENV-&gt;rep_set_priority()</a> </span>  
The <a href="../../api/c/reppriority.md" class="olink">DB_ENV-&gt;rep_set_priority()</a> method configures the local site's priority for the purpose of elections.

<span class="term"> <a href="../../api/c/repset_timeout.md" class="olink">DB_ENV-&gt;rep_set_timeout()</a> </span>  
This method optionally configures various timeout values. Otherwise default timeout values as specified in <a href="../../api/c/repset_timeout.md" class="olink">DB_ENV-&gt;rep_set_timeout()</a> are used. In particular, Replication Manager client sites can be configured to monitor the health of the TCP/IP connection to the master site using heartbeat messages. If the client receives no messages from the master for a certain amount of time, it considers the connection to be broken, and calls for an election to choose a new master. Heartbeat messages also help clients request missing master changes in the absence of master activity.

<span class="term"> <a href="../../api/c/envevent_notify.md" class="olink">DB_ENV-&gt;set_event_notify()</a> </span>  
Once configured and started, Replication Manager does virtually all of its work in the background, usually without the need for any direct communication with the application. However, occasionally events occur which the application may be interested in knowing about. The application can request notification of these events by calling the <a href="../../api/c/envevent_notify.md" class="olink">DB_ENV-&gt;set_event_notify()</a> method.

<span class="term"> <a href="../../api/c/repmgrstart.md" class="olink">DB_ENV-&gt;repmgr_start()</a> </span>  
The <a href="../../api/c/repmgrstart.md" class="olink">DB_ENV-&gt;repmgr_start()</a> method starts the replication system. It opens the listening TCP/IP socket and creates all the background processing threads that will be needed.

In addition to the methods previously described, Replication Manager applications may also call the following methods, as needed: <a href="../../api/c/repconfig.md" class="olink">DB_ENV-&gt;rep_set_config()</a>, <a href="../../api/c/repset_limit.md" class="olink">DB_ENV-&gt;rep_set_limit()</a>, <a href="../../api/c/repset_request.md" class="olink">DB_ENV-&gt;rep_set_request()</a>, <a href="../../api/c/repsync.md" class="olink">DB_ENV-&gt;rep_sync()</a> and <a href="../../api/c/repstat.md" class="olink">DB_ENV-&gt;rep_stat()</a>.

Finally, Replication Manager applications can also make use of the Replication Manager's message channels. This allows the various sites in the replication group to pass messages that are tailored to the application's requirements. For more information, see <a href="repmgr_channels.md" class="xref" title="Using Replication Manager message channels">Using Replication Manager message channels</a>.
