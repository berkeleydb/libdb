---
title: "Replication"
api-name: "Replication"
source: docs/upgrading/upgrade_4_3_repl.html
---
## Replication

The 4.3 release removes support for logs-only replication clients. Use of the DB_REP_LOGSONLY flag to the <a href="../../api/c/repstart.md" class="olink">DB_ENV-&gt;rep_start()</a> should be replaced with the <a href="../../api/c/repmgrstart.md#repmgrstart_DB_REP_CLIENT" class="olink">DB_REP_CLIENT</a> flag.

The 4.3 release adds two new arguments to the <a href="../../api/c/repelect.md" class="olink">DB_ENV-&gt;rep_elect()</a> method, **nvotes** and **flags**. The **nvotes** argument sets the required number of replication group members that must participate in an election in order for a master to be declared. For backward compatibility, set the **nvotes** argument to 0. The flags argument is currently unused and should be set to 0. See <a href="../../api/c/repelect.md" class="olink">DB_ENV-&gt;rep_elect()</a> method or "Replication Elections" for more information.

In the 4.3 release it is no longer necessary to do a database environment hot backup to initialize a replication client. All that is needed now is for the client to join the replication group. Berkeley DB will perform an internal backup from the master to the client automatically and will run recovery on the client to bring it up to date with the master.
