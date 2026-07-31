---
title: "Replication Events"
api-name: "Replication Events"
source: docs/upgrading/upgrade_4_6_event.html
---
## Replication Events

It is now guaranteed the <a href="../../api/c/envevent_notify.md#event_notify_DB_EVENT_REP_STARTUPDONE" class="olink">DB_EVENT_REP_STARTUPDONE</a> event will be presented to the application after the corresponding <a href="../../api/c/envevent_notify.md#event_notify_DB_EVENT_REP_NEWMASTER" class="olink">DB_EVENT_REP_NEWMASTER</a> event, even in the face of extreme thread-scheduling anomalies. (In previous releases, if the thread processing the NEWMASTER message was starved, and STARTUPDONE occurred soon after, the order might have been reversed.)

In addition, the <a href="../../api/c/envevent_notify.md#event_notify_DB_EVENT_REP_NEWMASTER" class="olink">DB_EVENT_REP_NEWMASTER</a> event is now presented to all types of replication applications: users of either the Replication Framework or the Base Replication API. In both cases, the <a href="../../api/c/envevent_notify.md#event_notify_DB_EVENT_REP_NEWMASTER" class="olink">DB_EVENT_REP_NEWMASTER</a> event always means that a site other than the local environment has become master.

The **envid** parameter to <a href="../../api/c/repmessage.md" class="olink">DB_ENV-&gt;rep_process_message()</a> has been changed to be of type "int" rather than "int \*", and the environment ID of a new master is presented to the application along with the <a href="../../api/c/envevent_notify.md#event_notify_DB_EVENT_REP_NEWMASTER" class="olink">DB_EVENT_REP_NEWMASTER</a> event. Replication applications should be modified to use the <a href="../../api/c/envevent_notify.md#event_notify_DB_EVENT_REP_NEWMASTER" class="olink">DB_EVENT_REP_NEWMASTER</a> event to determine the ID of the new master.

The **envid** parameter has been removed from the <a href="../../api/c/repelect.md" class="olink">DB_ENV-&gt;rep_elect()</a> method and a new event type has been added. The <a href="../../api/c/envevent_notify.md#event_notify_DB_EVENT_REP_ELECTED" class="olink">DB_EVENT_REP_ELECTED</a> event is presented to the application at the site which wins an election. In the Berkeley DB 4.6 release, the normal result of a successful election is either the <a href="../../api/c/envevent_notify.md#event_notify_DB_EVENT_REP_NEWMASTER" class="olink">DB_EVENT_REP_NEWMASTER</a> event (with the winner's environment ID), or the <a href="../../api/c/envevent_notify.md#event_notify_DB_EVENT_REP_ELECTED" class="olink">DB_EVENT_REP_ELECTED</a> event. Only one of the two events will ever be delivered.

The DB_REP_NEWMASTER return code has been removed from the <a href="../../api/c/repmessage.md" class="olink">DB_ENV-&gt;rep_process_message()</a> method. Replication applications should be modified to use the <a href="../../api/c/envevent_notify.md#event_notify_DB_EVENT_REP_NEWMASTER" class="olink">DB_EVENT_REP_NEWMASTER</a> and <a href="../../api/c/envevent_notify.md#event_notify_DB_EVENT_REP_ELECTED" class="olink">DB_EVENT_REP_ELECTED</a> events to determine the existence of a new master.
