---
title: "DB_ENV->set_paniccall"
api-name: "DB_ENV->set_paniccall"
source: docs/upgrading/upgrade_4_5_paniccall.html
---
## DB_ENV-\>set_paniccall

In previous Berkeley DB releases, the DB_ENV-\>set_paniccall and DB-\>set_paniccall methods were used to register a callback function, called if the database environment failed. In the 4.5 release, this functionality has been replaced by a general-purpose event notification callback function, set with the <a href="../../api/c/envevent_notify.md" class="olink">DB_ENV-&gt;set_event_notify()</a> method. Applications should be updated to replace DB_ENV-\>set_paniccall and DB-\>set_paniccall calls with a call to <a href="../../api/c/envevent_notify.md" class="olink">DB_ENV-&gt;set_event_notify()</a>. This also requires the callback function itself change, as the callback signatures are different.

The DB_ENV-\>set_paniccall and DB-\>set_paniccall calls are expected to be removed in a future release of Berkeley DB.
