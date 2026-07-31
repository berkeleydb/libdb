---
title: "Initializing a new site"
api-name: "Initializing a new site"
source: docs/programmer_reference/rep_init.html
---
## Initializing a new site

By default, adding a new site to a replication group only requires the client to join. Berkeley DB will automatically perform internal initialization from the master to the client, bringing the client into sync with the master.

However, depending on the network and infrastructure, it can be advantageous in a few instances to use a "hot backup" to initialize a client into a replication group. Clients not wanting to automatically perform internal initialization should call the <a href="../../api/c/repconfig.md" class="olink">DB_ENV-&gt;rep_set_config()</a> method to turn off the <a href="../../api/c/repconfig.md#config_DB_REP_CONF_AUTOINIT" class="olink">DB_REP_CONF_AUTOINIT</a> flag. Turning off this configuration flag causes Berkeley DB to return <a href="../../api/c/repmessage.md#repmsg_DB_REP_JOIN_FAILURE" class="olink">DB_REP_JOIN_FAILURE</a> to the application's <a href="../../api/c/repmessage.md" class="olink">DB_ENV-&gt;rep_process_message()</a> method instead of performing internal initialization.

To use a hot backup to initialize a client into a replication group, perform the following steps:

1.  Do an archival backup of the master's environment, as described in <a href="transapp_archival.md" class="xref" title="Database and log file archival">Database and log file archival</a>. The backup can either be a conventional backup or a hot backup.
2.  Copy the archival backup into a clean environment directory on the client.
3.  Run catastrophic recovery on the client's new environment, as described in <a href="transapp_recovery.md" class="xref" title="Recovery procedures">Recovery procedures</a>.
4.  Reconfigure and reopen the environment as a client member of the replication group.

If copying the backup to the client takes a long time relative to the frequency with which log files are reclaimed using the <a href="../../api/c/db_archive.md" class="olink">db_archive</a> utility or the <a href="../../api/c/logarchive.md" class="olink">DB_ENV-&gt;log_archive()</a> method, it may be necessary to suppress log reclamation until the newly restarted client has "caught up" and applied all log records generated during its downtime.

As with any Berkeley DB application, the database environment must be in a consistent state at application startup. This is most easily assured by running recovery at startup time in one thread or process; it is harmless to do this on both clients and masters even when not strictly necessary.
