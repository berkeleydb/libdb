---
title: "DB_REP_FULL_ELECTION"
api-name: "DB_REP_FULL_ELECTION"
source: docs/upgrading/upgrade_4_6_full_election.html
---
## DB_REP_FULL_ELECTION

The DB_REP_FULL_ELECTION flag historically specified to the <a href="../../api/c/repmgrstart.md" class="olink">DB_ENV-&gt;repmgr_start()</a> method has been removed from the 4.6 release.

In the Berkeley DB 4.6 release, a simpler and more flexible implementation of this functionality is available. Applications needing to configure the first election of a replication group differently from subsequent elections should use the <a href="../../api/c/repset_timeout.md#set_timeout_DB_REP_FULL_ELECTION_TIMEOUT" class="olink">DB_REP_FULL_ELECTION_TIMEOUT</a> flag to the <a href="../../api/c/repset_timeout.md" class="olink">DB_ENV-&gt;rep_set_timeout()</a> method to specify a different timeout for the first election.
