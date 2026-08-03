---
title: "Replication events"
api-name: "Replication events"
source: docs/upgrading/upgrade_4_5_rep_event.html
---
## Replication events

One of the informational returns from the <a href="../../api/c/repmessage.md" class="olink">DB_ENV-&gt;rep_process_message()</a> method found in previous releases of Berkeley DB has been changed to an event. The DB_REP_STARTUPDONE return from <a href="../../api/c/repmessage.md" class="olink">DB_ENV-&gt;rep_process_message()</a> is now the <a href="../../api/c/envevent_notify.md#event_notify_DB_EVENT_REP_STARTUPDONE" class="olink">DB_EVENT_REP_STARTUPDONE</a> value to the <a href="../../api/c/envevent_notify.md" class="olink">DB_ENV-&gt;set_event_notify()</a> callback.

Applications should update their handling of this event as necessary.
