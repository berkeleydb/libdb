---
title: "Chapter 10.  Replication Methods"
api-name: "Chapter 10.  Replication Methods"
source: docs/api_reference/C/rep.html
---
## Chapter 10.  Replication Methods

This chapter describes the APIs available to build Berkeley DB replicated applications. There are two different ways to build replication into a Berkeley DB application, and the APIs for both are described in this chapter.

For an overview of the two different ways to build a replicated application, see the *Berkeley DB Getting Started with Replicated Applications* guide.

The first, and simplest, way to build a replication Berkeley DB application is via the <span class="emphasis">*Replication Manager*</span>. If the Replication Manager does not meet your application's architectural requirements, you can write your own replication implementation using the "Base APIs".

Note that the Replication Manager is written using the Base APIs.

Note, also, that applications which make use of the Replication Manager use many of the Base APIs as the situation warrants. That said, a few Base API methods cannot be used by applications that are making use of the Replication Manager. Where this is the case, this is noted in the following method descriptions.

Finally, Replication Manager applications use the <a href="db_site.md" class="link" title="The DB_SITE Handle">DB_SITE</a> class to manage and configure replication sites. This handle is not used in any way by Base API applications.

## Replication and Related Methods

<table data-border="1" width="80%">
<thead>
<tr>
<th>Replication Manager Methods</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr>
<td><a href="dbchannel_close.md" class="xref" title="DB_CHANNEL-&gt;close()">DB_CHANNEL-&gt;close()</a></td>
<td>Closes a DB_CHANNEL handle</td>
</tr>
<tr>
<td><a href="dbchannel_send_msg.md" class="xref" title="DB_CHANNEL-&gt;send_msg()">DB_CHANNEL-&gt;send_msg()</a></td>
<td>Sends an asynchronous message on a DB_CHANNEL</td>
</tr>
<tr>
<td><a href="dbchannel_send_request.md" class="xref" title="DB_CHANNEL-&gt;send_request()">DB_CHANNEL-&gt;send_request()</a></td>
<td>Sends a synchronous message on a DB_CHANNEL</td>
</tr>
<tr>
<td><a href="dbchannel_set_timeout.md" class="xref" title="DB_CHANNEL-&gt;set_timeout()">DB_CHANNEL-&gt;set_timeout()</a></td>
<td>Sets the default timeout for the DB_CHANNEL</td>
</tr>
<tr>
<td><a href="repmgr_channel.md" class="xref" title="DB_ENV-&gt;repmgr_channel()">DB_ENV-&gt;repmgr_channel()</a></td>
<td>Creates a DB_CHANNEL handle</td>
</tr>
<tr>
<td><a href="repmgr_local_site.md" class="xref" title="DB_ENV-&gt;repmgr_local_site()">DB_ENV-&gt;repmgr_local_site()</a></td>
<td>Returns a DB_SITE handle for the local site</td>
</tr>
<tr>
<td><a href="repmgr_msg_dispatch.md" class="xref" title="DB_ENV-&gt;repmgr_msg_dispatch()">DB_ENV-&gt;repmgr_msg_dispatch()</a></td>
<td>Creates a DB_CHANNEL handle</td>
</tr>
<tr>
<td><a href="repmgrset_ack_policy.md" class="xref" title="DB_ENV-&gt;repmgr_set_ack_policy()">DB_ENV-&gt;repmgr_set_ack_policy()</a>, <a href="repmgrget_ack_policy.md" class="xref" title="DB_ENV-&gt;repmgr_get_ack_policy()">DB_ENV-&gt;repmgr_get_ack_policy()</a></td>
<td>Specify the Replication Manager's client acknowledgement policy</td>
</tr>
<tr>
<td><a href="repmgr_site.md" class="xref" title="DB_ENV-&gt;repmgr_site()">DB_ENV-&gt;repmgr_site()</a></td>
<td>Creates a DB_SITE handle</td>
</tr>
<tr>
<td><a href="repmgr_site_by_eid.md" class="xref" title="DB_ENV-&gt;repmgr_site_by_eid()">DB_ENV-&gt;repmgr_site_by_eid()</a></td>
<td>Creates a DB_SITE handle given an EID value</td>
</tr>
<tr>
<td><a href="repmgrsite_list.md" class="xref" title="DB_ENV-&gt;repmgr_site_list()">DB_ENV-&gt;repmgr_site_list()</a></td>
<td>List the sites and their status</td>
</tr>
<tr>
<td><a href="repmgrstart.md" class="xref" title="DB_ENV-&gt;repmgr_start()">DB_ENV-&gt;repmgr_start()</a></td>
<td>Start the Replication Manager</td>
</tr>
<tr>
<td><a href="repmgrstat.md" class="xref" title="DB_ENV-&gt;repmgr_stat()">DB_ENV-&gt;repmgr_stat()</a></td>
<td>Replication Manager statistics</td>
</tr>
<tr>
<td><a href="repmgrstat_print.md" class="xref" title="DB_ENV-&gt;repmgr_stat_print()">DB_ENV-&gt;repmgr_stat_print()</a></td>
<td>Print Replication Manager statistics</td>
</tr>
<tr>
<td colspan="2"><strong>Base API Methods</strong></td>
</tr>
<tr>
<td><a href="repelect.md" class="xref" title="DB_ENV-&gt;rep_elect()">DB_ENV-&gt;rep_elect()</a></td>
<td>Hold a replication election</td>
</tr>
<tr>
<td><a href="repmessage.md" class="xref" title="DB_ENV-&gt;rep_process_message()">DB_ENV-&gt;rep_process_message()</a></td>
<td>Process a replication message</td>
</tr>
<tr>
<td><a href="reptransport.md" class="xref" title="DB_ENV-&gt;rep_set_transport()">DB_ENV-&gt;rep_set_transport()</a></td>
<td>Configure replication transport callback</td>
</tr>
<tr>
<td><a href="repstart.md" class="xref" title="DB_ENV-&gt;rep_start()">DB_ENV-&gt;rep_start()</a></td>
<td>Start replication</td>
</tr>
<tr>
<td colspan="2"><strong>Additional Replication Methods</strong></td>
</tr>
<tr>
<td><a href="repstat.md" class="xref" title="DB_ENV-&gt;rep_stat()">DB_ENV-&gt;rep_stat()</a></td>
<td>Replication statistics</td>
</tr>
<tr>
<td><a href="repstat_print.md" class="xref" title="DB_ENV-&gt;rep_stat_print()">DB_ENV-&gt;rep_stat_print()</a></td>
<td>Print replication statistics</td>
</tr>
<tr>
<td><a href="repsync.md" class="xref" title="DB_ENV-&gt;rep_sync()">DB_ENV-&gt;rep_sync()</a></td>
<td>Replication synchronization</td>
</tr>
<tr>
<td><a href="envtxn_applied.md" class="xref" title="DB_ENV-&gt;txn_applied()">DB_ENV-&gt;txn_applied()</a></td>
<td>Check if a transaction has been replicated</td>
</tr>
<tr>
<td colspan="2"><strong>Replication Configuration</strong></td>
</tr>
<tr>
<td><a href="dbsite_close.md" class="xref" title="DB_SITE-&gt;close()">DB_SITE-&gt;close()</a></td>
<td>Closes the DB_SITE handle</td>
</tr>
<tr>
<td><a href="dbsite_get_address.md" class="xref" title="DB_SITE-&gt;get_address()">DB_SITE-&gt;get_address()</a></td>
<td>Returns a site's network address</td>
</tr>
<tr>
<td><a href="dbsite_get_eid.md" class="xref" title="DB_SITE-&gt;get_eid()">DB_SITE-&gt;get_eid()</a></td>
<td>Returns a site's Environment ID</td>
</tr>
<tr>
<td><a href="dbsite_remove.md" class="xref" title="DB_SITE-&gt;remove()">DB_SITE-&gt;remove()</a></td>
<td>Removes the site from the replication group</td>
</tr>
<tr>
<td><a href="dbsite_set_config.md" class="xref" title="DB_SITE-&gt;set_config()">DB_SITE-&gt;set_config()</a>, <a href="dbsite_get_config.md" class="xref" title="DB_SITE-&gt;get_config()">DB_SITE-&gt;get_config()</a></td>
<td>Configure a DB_SITE handle</td>
</tr>
<tr>
<td><a href="repclockskew.md" class="xref" title="DB_ENV-&gt;rep_set_clockskew()">DB_ENV-&gt;rep_set_clockskew()</a>, <a href="repget_clockskew.md" class="xref" title="DB_ENV-&gt;rep_get_clockskew()">DB_ENV-&gt;rep_get_clockskew()</a></td>
<td>Configure master lease clock adjustment</td>
</tr>
<tr>
<td><a href="repconfig.md" class="xref" title="DB_ENV-&gt;rep_set_config()">DB_ENV-&gt;rep_set_config()</a>, <a href="repget_config.md" class="xref" title="DB_ENV-&gt;rep_get_config()">DB_ENV-&gt;rep_get_config()</a></td>
<td>Configure the replication subsystem</td>
</tr>
<tr>
<td><a href="repset_limit.md" class="xref" title="DB_ENV-&gt;rep_set_limit()">DB_ENV-&gt;rep_set_limit()</a>, <a href="repget_limit.md" class="xref" title="DB_ENV-&gt;rep_get_limit()">DB_ENV-&gt;rep_get_limit()</a></td>
<td>Limit data sent in response to a single message</td>
</tr>
<tr>
<td><a href="repnsites.md" class="xref" title="DB_ENV-&gt;rep_set_nsites()">DB_ENV-&gt;rep_set_nsites()</a>, <a href="repget_nsites.md" class="xref" title="DB_ENV-&gt;rep_get_nsites()">DB_ENV-&gt;rep_get_nsites()</a></td>
<td>Configure replication group site count</td>
</tr>
<tr>
<td><a href="reppriority.md" class="xref" title="DB_ENV-&gt;rep_set_priority()">DB_ENV-&gt;rep_set_priority()</a>, <a href="repget_priority.md" class="xref" title="DB_ENV-&gt;rep_get_priority()">DB_ENV-&gt;rep_get_priority()</a></td>
<td>Configure replication site priority</td>
</tr>
<tr>
<td><a href="repset_request.md" class="xref" title="DB_ENV-&gt;rep_set_request()">DB_ENV-&gt;rep_set_request()</a>, <a href="repget_request.md" class="xref" title="DB_ENV-&gt;rep_get_request()">DB_ENV-&gt;rep_get_request()</a></td>
<td>Configure replication client retransmission requests</td>
</tr>
<tr>
<td><a href="repset_timeout.md" class="xref" title="DB_ENV-&gt;rep_set_timeout()">DB_ENV-&gt;rep_set_timeout()</a>, <a href="repget_timeout.md" class="xref" title="DB_ENV-&gt;rep_get_timeout()">DB_ENV-&gt;rep_get_timeout()</a></td>
<td>Configure replication timeouts</td>
</tr>
<tr>
<td colspan="2"><strong>Transaction Operations</strong></td>
</tr>
<tr>
<td><a href="txnset_commit_token.md" class="xref" title="DB_TXN-&gt;set_commit_token()">DB_TXN-&gt;set_commit_token()</a></td>
<td>Set a commit token</td>
</tr>
</tbody>
</table>
